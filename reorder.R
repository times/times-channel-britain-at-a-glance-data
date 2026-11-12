# Reorder the sparklines without refetching any data.
#
#   Rscript reorder.R "NHS waiting list" "Real GDP per capita" "gdp growth"
#
# Moves the given labels to the top, in that order, leaving everything else in
# its current order. Updates both:
#   - sparklines-page.json / sparklines-slice.json (the `position` field), so the
#     change is live straight away and survives quick updates
#   - the full-branch bind_rows(list(...)) block in script.R, so the next full
#     update produces the same order
# Labels are matched case-insensitively, and a partial name is fine if it only
# matches one label (e.g. "gdp growth" -> "GDP growth (annual rate)").

suppressMessages(library(jsonlite))

requested <- commandArgs(trailingOnly = TRUE)
if (length(requested) == 0) stop('Usage: Rscript reorder.R "Label one" "Label two" ...')

normalise <- function(x) gsub('[^a-z0-9]', '', tolower(x))

match_label <- function(query, labels) {
  exact <- labels[tolower(labels) == tolower(query)]
  if (length(exact) == 1) return(exact)
  partial <- labels[grepl(normalise(query), normalise(labels), fixed = TRUE)]
  if (length(partial) == 1) return(partial)
  if (length(partial) == 0) stop('No label matches "', query, '"')
  stop('"', query, '" matches more than one label: ', paste(partial, collapse = ', '))
}

move_to_top <- function(current, top) c(top, setdiff(current, top))

# --- JSON ---
records <- read_json('sparklines-page.json', simplifyVector = FALSE)
records <- records[order(sapply(records, function(r) r$position))]
json_labels <- sapply(records, function(r) r$label)

top <- unique(sapply(requested, match_label, labels = json_labels, USE.NAMES = FALSE))
new_json_order <- move_to_top(json_labels, top)

records <- records[match(new_json_order, json_labels)]
for (i in seq_along(records)) records[[i]]$position <- i

# --- script.R bind_rows() block ---
src <- readLines('script.R')
start <- grep('^  master <- bind_rows\\(list\\($', src)
if (length(start) != 1) stop('Could not find the full-branch bind_rows(list( block in script.R')
end <- start + which(grepl('^  \\)\\) %>%', src[(start + 1):length(src)]))[1]
block <- src[(start + 1):(end - 1)]

# Each series entry starts with a 4-space-indented line (e.g. "    inf %>%")
item_starts <- grep('^    [^ ]', block)
items <- lapply(seq_along(item_starts), function(i) {
  last <- if (i < length(item_starts)) item_starts[i + 1] - 1 else length(block)
  block[item_starts[i]:last]
})
item_labels <- sapply(items, function(it) {
  m <- regmatches(it, regexpr("label = '[^']+'", it))
  if (length(m) != 1) stop('Could not find a single label in bind_rows() entry:\n', paste(it, collapse = '\n'))
  sub("label = '([^']+)'", '\\1', m)
})

missing_in_script <- setdiff(top, item_labels)
if (length(missing_in_script)) stop('Not found in script.R bind_rows(): ', paste(missing_in_script, collapse = ', '))

new_script_order <- move_to_top(item_labels, top)
items <- items[match(new_script_order, item_labels)]
items <- lapply(seq_along(items), function(i) {
  it <- items[[i]]
  n <- length(it)
  it[n] <- sub(',\\s*$', '', it[n])
  if (i < length(items)) it[n] <- paste0(it[n], ',')
  it
})

if (!identical(new_json_order, new_script_order)) {
  warning('JSON and script.R orders differ (labels out of sync?). Both were updated, but run a full update to realign.')
}

# --- Write ---
write_json(records, 'sparklines-page.json', auto_unbox = T, pretty = T)
write_json(records, 'sparklines-slice.json', auto_unbox = T, pretty = T)
writeLines(c(src[1:start], unlist(items), src[end:length(src)]), 'script.R')

message('New order (top ', min(length(new_json_order), length(top) + 3), '):')
message(paste0('  ', seq_len(min(length(new_json_order), length(top) + 3)), '. ',
               head(new_json_order, length(top) + 3), collapse = '\n'))
