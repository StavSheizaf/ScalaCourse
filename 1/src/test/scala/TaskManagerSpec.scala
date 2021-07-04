
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper


class TaskManagerSpec extends AnyFlatSpec with Matchers {

  "An empty tasks list" should "have 0 tasks due today" in {
    val tasksDueToday: List[String] = Nil
    tasksDueToday should have length 0
  }

}

