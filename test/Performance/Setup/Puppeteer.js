import P from 'puppeteer'

const sessions = new WeakMap()

export function launchImpl (args) {
  return function () {
    return P.launch(args)
  }
}

export async function newPageImpl (browser) {
  const page = await browser.newPage()
  sessions.set(page, await page.createCDPSession())
  return page
}

export function debugImpl (page) {
  page.on('console', msg => console.log('PAGE LOG:', msg.text()))
  page.on('pageerror', err => console.log('ERROR LOG:', err.message))
}

export function clickImpl (elem) {
  return elem.click()
}

export function waitForSelectorImpl (page, selector) {
  return page.waitForSelector(selector)
}

export function focusImpl (page, selector) {
  return page.focus(selector)
}

export function typeWithKeybordImpl (page, string) {
  return page.keyboard.type(string)
}

export function gotoImpl (page, path) {
  return page.goto(path)
}

export async function closePageImpl (page) {
  await sessions.get(page).detach()
  sessions.delete(page)
  await page.close()
}

export function closeBrowserImpl (browser) {
  return browser.close()
}

export function enableHeapProfilerImpl (page) {
  return sessions.get(page).send('HeapProfiler.enable')
}

export function collectGarbageImpl (page) {
  return sessions.get(page).send('HeapProfiler.collectGarbage')
}

export async function startTraceImpl (page, path) {
  await page.tracing.start({ path })
  await page.evaluate(() => {
    const sample = { timestamps: [], request: 0 }
    window.__hooksFrameSample = sample
    const frame = timestamp => {
      sample.timestamps.push(timestamp)
      sample.request = requestAnimationFrame(frame)
    }
    sample.request = requestAnimationFrame(frame)
  })
}

export async function stopTraceImpl (page) {
  const timestamps = await page.evaluate(() => {
    const sample = window.__hooksFrameSample
    cancelAnimationFrame(sample.request)
    delete window.__hooksFrameSample
    return sample.timestamps
  })
  await page.tracing.stop()
  if (timestamps.length < 2) {
    throw new Error('Not enough animation frames to measure FPS')
  }
  return Math.round(1000 * (timestamps.length - 1) / (timestamps.at(-1) - timestamps[0]))
}

export function pageMetricsImpl (page) {
  return page.metrics()
}
