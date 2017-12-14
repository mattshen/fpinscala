import fpinscala.parallelism.Examples._
import java.util.concurrent.Executors

Examples.sum(1 to 1000)(Executors.newCachedThreadPool).get