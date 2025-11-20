# Evaluating Different Trading Strategies on PFE Stock <br><br>

A university-based assignment covering simple time-series diagonistics to look for insights in patterns that could be valuable for determining what type of trading strategy to use later on. We discovered through the ACF/PACF plots that there was no autocorrelation present (generally found white noise).<br><br>

Backtested a cross moving average strategy and bollinger bands strategy (with the BB functions created using Gen-AI). <br><br>

The prompt utilised was the following:<br>

Write clean, well-commented R code to implement a Bollinger Bands breakout trading strategy. Assume the dataset `pfe` contains `datadate` (Date) and `prccd` (closing price).

The script must:

1. Define a function to compute Bollinger Bands (middle, upper, lower) using parameters `n` and `k`.
2. Create a function to generate trading signals (+1 long, −1 short, 0 neutral) based on Bollinger breakout logic.
3. Include a backtesting function that computes daily strategy returns and an equity curve, using lagged signals to prevent lookahead bias.
4. Compare the strategy to a buy-and-hold benchmark and produce an interpretable performance summary including an equity curve plot.
5. Ensure code is readable, well-structured, and appropriate for use in a technical report.

Overall, found that Cross-MA was the most profitable strategy and the following show a few of the time-series plots from the project.

<div align="center" style="background-color:white; padding:20px; border-radius:10px;">

<table>
  <tr>
    <td align="center" style="padding:10px;">
      <img src="https://github.com/hivan04/pfe-trading-strats/blob/main/images/bb-eval.png" width="400"><br>
      <b>Bollinger Bands Evaluation</b>
    </td>
    <td align="center" style="padding:10px;">
      <img src="https://github.com/hivan04/pfe-trading-strats/blob/main/images/cma-eval.png" width="400"><br>
      <b>CMA Strategy Evaluation</b>
    </td>
  </tr>
  <tr>
    <td align="center" style="padding:10px;">
      <img src="https://github.com/hivan04/pfe-trading-strats/blob/main/images/bb-bs-points.png" width="400"><br>
      <b>Bollinger Bands Buy/Sell Points</b>
    </td>
    <td align="center" style="padding:10px;">
      <img src="https://github.com/hivan04/pfe-trading-strats/blob/main/images/cma-crossover.png" width="400"><br>
      <b>CMA Crossover Strategy</b>
    </td>
  </tr>
</table>

</div>


