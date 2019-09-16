object Result {

  val CHARGE = "CHARGE:"
  val BLOCK = "BLOCK:"
  val ALLOW = "ALLOW:"

  val COUNTRY = "card_country"
  val CURRENCY = "currency"
  val AMOUNT = "amount"
  val IP = "ip_country"

  val variables: scala.collection.immutable.Set[String] =
    scala.collection.immutable.Set(COUNTRY, IP, CURRENCY, AMOUNT)

  def body(rule: String): String = {
    val prefix = rule(0) match {
      case 'A' => ALLOW
      case 'B' => BLOCK
      case 'C' => CHARGE
    }
    rule.drop(prefix.length).trim
  }

  def parts(line: String, delim: String): Array[String] =
    line.replaceAll("\\s+", "").split(delim)

  def parseCharge(line: String): scala.collection.immutable.Map[String, String] =
    parts(line, "&").map(_.split("=")).map(it => (it(0), it(1))).toMap

  def evalRule(line: String, charge: scala.collection.immutable.Map[String, String]): Boolean =
    if (line.contains(" AND ")) evalAnd(line, charge)
    else if (line.contains(" OR ")) evalOr(line, charge)
    else eval(line, charge)

  def eval(rule: String, charge: scala.collection.immutable.Map[String, String]): Boolean =
    if (rule.contains("==")) evalEq(rule, charge)
    else evalNe(rule, charge)

  def evalEq(rule: String, charge: scala.collection.immutable.Map[String, String]): Boolean = {
    val Array(lhs, rhs) = parts(rule, "==").map(it => substituteValue(it, charge))
    lhs == rhs
  }

  def evalNe(rule: String, charge: scala.collection.immutable.Map[String, String]): Boolean = {
    val Array(lhs, rhs) = parts(rule, "!=").map(it => substituteValue(it, charge))
    lhs != rhs
  }

  def substituteValue(
    exp: String,
    charge: scala.collection.immutable.Map[String, String]
  ): String = {
    val result = charge.getOrElse(exp, exp)
    result
  }

  def evalAnd(line: String, charge: scala.collection.immutable.Map[String, String]): Boolean = {
    val parts = line.split(" AND ")
    eval(parts(0), charge) && eval(parts(1), charge)
  }

  def evalOr(line: String, charge: scala.collection.immutable.Map[String, String]): Boolean = {
    val parts = line.split(" OR ")
    eval(parts(0), charge) || eval(parts(1), charge)
  }

  def evalRules(
    rules: Array[String],
    charge: scala.collection.immutable.Map[String, String],
    ruleType: String
  ): Array[Boolean] =
    rules
      .map(_.trim)
      .filter(_.startsWith(ruleType))
      .map(body)
      .map(it => evalRule(it, charge))

  def should_allow_charge(charge_and_rules: Array[String]): Boolean = {
    if (charge_and_rules.length <= 1) return true
    val charge = parseCharge(body(charge_and_rules.head))
    val rules = charge_and_rules.drop(1)
    val allow = evalRules(rules, charge, ALLOW)
    val block = evalRules(rules, charge, BLOCK)

    block.isEmpty || allow.exists(it => it) || block.forall(it => !it)
  }

  def main(args: Array[String]): Unit = {
    val in = Array(
      "CHARGE: currency=MYR&ip_country=MY&card_country=US&amount=100",
      "ALLOW: card_country == US AND ip_country != US ",
      "BLOCK: amount==100 OR ip_country == MY"
    )

    println(should_allow_charge(in))
  }
}
