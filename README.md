# fmeasy
Repository for "SAT-based Analysis of Large Real-world Feature Models is Easy"

## Authors
1. Jia	Hui Liang
2. [Vijay Ganesh](https://ece.uwaterloo.ca/~vganesh)
3. [Venkatesh Raman](http://www.imsc.res.in/~vraman)
4. [Krzysztof Czarnecki](http://gsd.uwaterloo.ca/kczarnec)

For help, support, feedback, or comments feel free to contact the following email address: jliang@gsd.uwaterloo.ca or vganesh@uwaterloo.ca. 

## Abstract
Modern conflict-driven clause-learning (CDCL) Boolean SAT solvers provide efficient automatic analysis of real-world feature models (FM) of systems ranging from cars to operating systems. It is well-known that solver-based analysis of real-world FMs scale very well even though SAT instances obtained from such FMs are large, and the corresponding analysis problems are known to be NP-complete. To better understand why SAT solvers are so effective, we systematically studied many syntactic and semantic characteristics of a representative set of large real-world FMs. We discovered that a key reason why large real-world FMs are easy-to-analyze is that the vast majority of the variables in these models are *unrestricted*, i.e., the models are satisfiable for both true and false assignments to such variables under the current partial assignment. Given this discovery and our understanding of CDCL SAT solvers, we show that solvers can easily find satisfying assignments for such models without too many backtracks relative to the model size, explaining why solvers scale so well. Further analysis showed that the presence of unrestricted variables in these real-world models can be attributed to their high-degree of *variability*. Additionally, we experimented with a series of well-known non-backtracking simplifications that are particularly effective in solving FMs. The remaining variables/clauses after simplifications, called the *core*, are so few that they are easily solved even with backtracking, further strengthening our conclusions. We explain the connection between our findings and *backdoors*, an idea posited by theorists to explain the power of SAT solvers. This connection strengthens our hypothesis that SAT-based analysis of FMs is easy. In contrast to our findings, previous research characterizes the difficulty of analyzing randomly-generated FMs in terms of treewidth. Our experiments suggest that the difficulty of analyzing real-world FMs cannot be explained in terms of treewidth.
