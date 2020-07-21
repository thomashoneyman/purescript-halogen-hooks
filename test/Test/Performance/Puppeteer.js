const puppeteer = require("puppeteer");

(async () => {
  const browser = await puppeteer.launch();

  const page = await browser.newPage();
  await page.goto("file:///Users/thoneyman/Code/hooks/test/test.html");

  await page.tracing.start({ path: "trace.json" });
  const testA = await page.waitForSelector("#a");

  // Hook test
  a = await measure(page, async () => {
    await testA.click();
    await page.waitForSelector("#completed");
  });

  b = await measure(page, async () => {
    await testA.click();
    await page.waitForSelector("#completed");
  });

  await page.tracing.stop();

  await browser.close();

  console.log(
    "JSHeapUsedSize",
    a.JSHeapUsedSize / 1000,
    b.JSHeapUsedSize / 1000
  );

  console.log("JSSecondsElapsed", a.Timestamp, b.Timestamp);
})();

measure = async (page, testFunction) => {
  /** part (1) - garbage collect and get metrics from the page **/
  await page._client.send(`HeapProfiler.enable`);
  await page._client.send(`HeapProfiler.collectGarbage`);
  const startMetrics = await page.metrics();

  /** part (2) - execute the test function **/
  await testFunction();

  /** part (3) - garbage collect and get metrics from the page again **/
  await page._client.send(`HeapProfiler.collectGarbage`);
  const metrics = await page.metrics();

  /** part (4) - calculate differences before and after excuting the test function **/
  const measures = [
    `JSHeapUsedSize`,
    `LayoutCount`,
    `RecalcStyleCount`,
    `JSEventListeners`,
    `Nodes`,
    `ScriptDuration`,
    `TaskDuration`,
    `Timestamp`,
    `LayoutDuration`,
    `RecalcStyleDuration`,
  ].reduce(
    (accumulator, metric) => ({
      ...accumulator,
      [metric]: metrics[metric] - startMetrics[metric],
    }),
    {}
  );

  return measures;
};
