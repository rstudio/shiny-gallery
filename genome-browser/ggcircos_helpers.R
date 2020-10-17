
add_circles <- function(vis, track_radians, ...) {
  
  UseMethod("add_circles", track_radians)
  
}


add_circles.numeric <- function(vis, track_radians, r, ..., interpolate = "linear-open") {
  
  df <- data.frame(name = rep(names(track_radians), length(r)), theta = rep(track_radians, length(r)), r = rep(r, each = length(track_radians)))
  
  vis %>% layer_paths(data = df %>% group_by(r, name), ~sin(theta) * r, ~cos(theta) * r, interpolate := interpolate, ...)
  
}

add_circles.reactive <- function(vis, track_radians, r, ..., interpolate = "linear-open") {
  
  df <- shiny::reactive({
    
    data.frame(name = rep(names(track_radians()), length(r)), theta = rep(track_radians(), length(r)), r = rep(r, each = length(track_radians())))
    
  })
  
  vis %>% layer_paths(data = df %>% group_by(r, name), ~sin(theta) * r, ~cos(theta) * r, interpolate := interpolate, ...)
  
}



add_lines <- function(vis, seq_df) {
  
  UseMethod("add_lines", seq_df)
  
}

add_lines.default <- function(vis, seq_df, seq, position, value, outer, inner, ...,
                              data = NULL, max_value = NULL, min_value = NULL) {
  
  
  seq_sub <- eval(substitute(seq), data, parent.frame())
  position_sub <- eval(substitute(position), data, parent.frame())
  value_sub <- eval(substitute(value), data, parent.frame())
  
  
  if(is.null(max_value)) max_value <- max(value_sub) 
  if(is.null(min_value)) min_value <- min(value_sub)
  
  df <- fit_lines(seq_sub, position_sub, value_sub, outer, inner, seq_df, max_value, min_value) %>% 
    group_by(seq) %>%
    arrange(theta)
  
  vis %>% layer_paths(data = df, ~sin(theta) * r, ~cos(theta) * r, ...)
  
}


add_lines.reactive <- function(vis, seq, position, value, outer, inner, seq_df, ...,
                               data = NULL, max_value = NULL, min_value = NULL) {
  
  df <- shiny::reactive({
    
    if (shiny::is.reactive(data)) {
      data <- data()
    }
    
    seq_sub <- eval(substitute(seq, parent_env(environment())), data, parent.frame())
    position_sub <- eval(substitute(position, parent_env(environment())), data, parent.frame())
    value_sub <- eval(substitute(value, parent_env(envirnomnet())), data, parent.frame())
    
    if(is.null(max_value)) max_value <- max(value_sub) 
    if(is.null(min_value)) min_value <- min(value_sub)
    
    fit_lines(seq_sub, position_sub, value_sub, outer, inner, seq_df(), max_value, min_value) %>% group_by(seq) %>% arrange(theta)
    
  })
  
  vis %>% layer_paths(data = df, ~sin(theta) * r, ~cos(theta) * r, ...)
  
}



add_links <- function(vis, seq_df, ...) {
  
  UseMethod("add_links", seq_df)
  
}

add_links.data.frame <- function(vis, seq_df, name_from, name_to, pos_from, pos_to,
                                 start_r, end_r, inner_r = 0.1, ..., data = NULL,
                                 interpolate = "basis") {
  
  
  name_from_sub <- eval(substitute(name_from), data, parent.frame())
  name_to_sub <- eval(substitute(name_to), data, parent.frame())
  pos_from_sub <- eval(substitute(pos_from), data, parent.frame())
  pos_to_sub <- eval(substitute(pos_to), data, parent.frame())
  
  links_df <- fit_links(name_from_sub, name_to_sub, pos_from_sub, pos_to_sub, seq_df, start_r, end_r, inner_r)
  
  vis %>% layer_paths(data = links_df %>% group_by(link), ~sin(theta) * r, ~cos(theta) * r, ..., interpolate := interpolate)
  
}

add_links.reactive <- function(vis, seq_df, name_from, name_to, pos_from, pos_to,
                               start_r, end_r, inner_r = 0.1, ..., data = NULL, interpolate = "basis") {
  
  
  
  links_df <- reactive({
    
    if (shiny::is.reactive(data)) {
      data <- data()
    }
    
    name_from_sub <- eval(substitute(name_from, parent.env(environment())), data, parent.frame())
    name_to_sub <- eval(substitute(name_to, parent.env(environment())), data, parent.frame())
    pos_from_sub <- eval(substitute(pos_from, parent.env(environment())), data, parent.frame())
    pos_to_sub <- eval(substitute(pos_to, parent.env(environment())), data, parent.frame())
    
    fit_links(name_from_sub, name_to_sub, pos_from_sub, pos_to_sub, seq_df(), start_r, end_r, inner_r)    
    
  })
  
  vis %>% layer_paths(data = links_df %>% group_by(link), ~sin(theta) * r, ~cos(theta) * r, ..., interpolate := interpolate)
  
}




add_points <- function(vis, seq_df, ...) {
  
  UseMethod("add_points", seq_df)
  
}

add_points.default <- function(vis, seq_df, seq, position, value, outer, inner, ..., data = NULL, metadata = NULL, max_value = NULL, min_value = NULL) {
  
  seq_sub <- eval(substitute(seq), data, parent.frame())
  position_sub <- eval(substitute(position), data, parent.frame())
  value_sub <- eval(substitute(value), data, parent.frame())
  
  
  if(is.null(max_value)) max_value <- max(value_sub) 
  if(is.null(min_value)) min_value <- min(value_sub)
  
  df <- fit_points(seq_sub, position_sub, value_sub, outer, inner, seq_df, metadata, max_value, min_value)
  
  vis %>% layer_points(data = df, ~sin(theta) * r, ~cos(theta) * r, ...)
  
}


add_points.reactive <- function(vis, seq_df, seq, position, value, outer, inner, ..., data = NULL, metadata = NULL, max_value = NULL, min_value = NULL) {
  
  
  
  df <- shiny::reactive({
    
    if (shiny::is.reactive(data)) {
      
      data <- data()
      
    }
    
    seq_sub <- eval(substitute(seq, parent.env(environment())), data, parent.frame())
    position_sub <- eval(substitute(position, parent.env(environment())), data, parent.frame())
    value_sub <- eval(substitute(value, parent.env(environment())), data, parent.frame())
    
    if(is.null(max_value)) max_value <- max(value_sub) 
    if(is.null(min_value)) min_value <- min(value_sub)
    
    
    fit_points(seq_sub, position_sub, value_sub, outer, inner, seq_df(), metadata, max_value, min_value)
    
  })
  
  vis %>% layer_points(data = df, ~sin(theta) * r, ~cos(theta) * r, ...)
  
}



add_ribbons <- function(vis, seq_df, ...) {
  
  UseMethod("add_ribbons", seq_df)
  
}


add_ribbons.default <- function(vis, seq_df, name_from, name_to, pos_from_start, pos_from_end,
                                pos_to_start, pos_to_end, start_r, end_r, inner_r = 0.1,
                                ..., data = NULL, interpolate = "basis") {
  
  name_from_sub <- eval(substitute(name_from), data, parent.frame())
  name_to_sub <- eval(substitute(name_to), data, parent.frame())
  pos_from_start_sub <- eval(substitute(pos_from_start), data, parent.frame())
  pos_from_end_sub <- eval(substitute(pos_from_end), data, parent.frame())
  pos_to_start_sub <- eval(substitute(pos_to_start), data, parent.frame())
  pos_to_end_sub <- eval(substitute(pos_to_end), data, parent.frame())
  
  ribbons_df <- fit_ribbons(name_from_sub, name_to_sub, pos_from_start_sub,
                            pos_from_end_sub, pos_to_start_sub, pos_to_end_sub,
                            seq_df, start_r, end_r, inner_r)
  
  vis %>% layer_paths(data = ribbons_df %>% group_by(link), ~sin(theta) * r, ~cos(theta) * r,
                      ..., interpolate := interpolate)
  
}


add_ribbons.reactive <- function(vis, seq_df, name_from, name_to, pos_from_start, pos_from_end,
                                 pos_to_start, pos_to_end, start_r, end_r, inner_r = 0.1,
                                 ..., data = NULL, metadata = NULL, interpolate = "basis") {
  
  
  
  ribbons_df <- reactive({
    
    if (shiny::is.reactive(data)) {
      data <- data()
    }
    
    name_from_sub <- eval(substitute(name_from, parent.env(environment())), data, parent.frame())
    name_to_sub <- eval(substitute(name_to, parent.env(environment())), data, parent.frame())
    pos_from_start_sub <- eval(substitute(pos_from_start, parent.env(environment())), data, parent.frame())
    pos_from_end_sub <- eval(substitute(pos_from_end, parent.env(environment())), data, parent.frame())
    pos_to_start_sub <- eval(substitute(pos_to_start, parent.env(environment())), data, parent.frame())
    pos_to_end_sub <- eval(substitute(pos_to_end, parent.env(environment())), data, parent.frame())
    
    fit_ribbons(name_from_sub, name_to_sub, pos_from_start_sub,
                pos_from_end_sub, pos_to_start_sub, pos_to_end_sub,
                seq_df(), start_r, end_r, inner_r)
    
  })
  
  vis %>% layer_paths(data = ribbons_df %>% group_by(link), ~sin(theta) * r, ~cos(theta) * r,
                      ..., interpolate := interpolate)
  
}


add_text <- function(vis, seq_df, ...) {
  
  UseMethod("add_text", seq_df)
  
}


add_text.default <- function(vis, seq_df, seq, position, label, r, ..., data = NULL) {
  
  seq_sub <- eval(substitute(seq), data, parent.frame())
  position_sub <- eval(substitute(position), data, parent.frame())
  label_sub <- eval(substitute(label), data, parent.frame())
  r_sub <- eval(substitute(r), data, parent.frame())
  
  text_df <- fit_to_seq(seq_sub, position_sub, seq_df, metadata = data.frame(label_sub, r_sub))
  
  vis %>% layer_text(data = text_df, ~sin(theta) * r_sub, ~cos(theta) * r_sub, text := ~label_sub, angle := ~theta * 180/pi, ...)
  
}


add_text.reactive <- function(vis, seq_df, seq, position, label, r, ..., data = NULL) {
  
  text_df <- reactive({
    
    if (shiny::is.reactive(data)) {
      data <- data()
    }
    
    seq_sub <- eval(substitute(seq, parent.env(environment())), data, parent.frame())
    position_sub <- eval(substitute(position, parent.env(environment())), data, parent.frame())
    label_sub <- eval(substitute(label, parent.env(environment())), data, parent.frame())
    r_sub <- eval(substitute(r, parent.env(environment())), data, parent.frame())
    
    fit_to_seq(seq_sub, position_sub, seq_df, metadata = data.frame(label_sub, r_sub))
    
  })
  
  vis %>% layer_text(data = text_df, ~sin(theta) * r, ~cos(theta) * r, text := ~label_sub, angle := ~theta * 180/pi, ...)
  
}










add_ticks <- function(vis, radians, outer, inner, ...) {
  
  df <- data.frame(theta = rep(radians, 2), r = c(rep(outer, length(radians)), rep(inner, length(radians))))
  
  vis %>% layer_paths(data = df %>% group_by(theta), ~sin(theta) * r, ~cos(theta) * r, ...)
  
}




add_track <- function(vis, track_radians, ...) {
  
  UseMethod("add_track", track_radians)
  
}

add_track.numeric <- function(vis, track_radians, outer, inner, ..., interpolate = "linear-closed") {
  
  track_df <- create_track_df(track_radians, outer, inner)
  
  vis %>% layer_paths(data = track_df %>% group_by(group), ~sin(theta) * r, ~cos(theta) * r, interpolate := interpolate, ...)
  
} 

add_track.reactive <- function(vis, track_radians, outer, inner, ..., interpolate = "linear-closed") {
  
  track_df <- shiny::reactive({
    
    create_track_df(track_radians(), outer, inner)
    
  })
  
  vis %>% layer_paths(data = track_df %>% group_by(group), ~sin(theta) * r, ~cos(theta) * r, interpolate := interpolate, ...)
  
}


create_radians <- function(seq = c(1:22, "X", "Y"),
                           lengths = c(249250621,243199373,198022430,191154276,180915260,171115067,
                                       159138663,146364022,141213431,135534747,135006516,133851895,
                                       115169878,107349540,102531392,90354753,81195210,78077248,
                                       59128983,63025520,48129895,51304566,155270560,59373566),
                           total_gap = 0.2,
                           ind_gaps = rep(total_gap / length(seq), length(seq))) {
  
  stand_lengths = lengths / sum(lengths)
  
  all_lengths <- c(rbind(ind_gaps, stand_lengths))
  
  all_stand_lengths <- all_lengths / sum(all_lengths)
  
  all_radians <- cumsum(all_stand_lengths) * 2 * pi
  
  names(all_radians) <- rep(seq, each = 2)
  
  attr(all_radians, "lengths") <- lengths
  
  return(all_radians)
  
}


create_seq_df <- function(radians, seq = unique(names(radians)),
                          lengths = attr(radians, "lengths"),
                          scale = 1) {
  
  seq_starts <- radians[seq(1, length(radians), 2)]
  seq_ends <- radians[seq(2, length(radians), 2)]
  
  data.frame(seq = seq, length = lengths, seq_start = seq_starts,
             seq_end = seq_ends, scale = scale)
  
}








create_track_df <- function(track_radians, outer, inner) {
  
  outer_df <- data.frame(r = outer, theta = track_radians, group = factor(names(track_radians), levels = unique(names(track_radians))))
  inner_df <- data.frame(r = inner, theta = track_radians, group = factor(names(track_radians), levels = unique(names(track_radians))))
  
  ## use interpolate := "linear-closed" in layer_paths to avoid the need to join up paths manually
  
  track_df <- rbind(outer_df, arrange(inner_df, -theta))
  
  return(track_df)
  
}


create_track_radians <- function(radians,
                                 seq = unique(names(radians)),
                                 approx_points = 400,
                                 points_per_track = rep(ceiling(approx_points / length(seq)), length(seq))) {
  
  track_radians <- do.call(c, lapply(seq_along(seq),
                                     function(i) {
                                       x <- c(seq(radians[2*i - 1], radians[2*i], length.out = points_per_track[i]))
                                       names(x) <- rep(seq[i], length(x))
                                       x
                                     }
  )
  )
  
  # having attributes on things which are passed to ggvis causes errors
  # could remove at a later point but this can be fixed an alternate way (see create_track_df)
  ##attr(track_radians, "seq") <- seq
  
  return(track_radians)
  
}



fit_lines <- function(seq, position, value, outer, inner, seq_df, max_value = NULL, min_value = NULL, between_points = 10) {
  
  new_seq <- function(seq, between_points) {
    c(rep(seq[-length(seq)], each = between_points - 1),
      seq[length(seq)]
    )
  }
  
  new_position <- function(position, between_points) {
    c(position[1],
      mapply(seq, lag(position)[-1], position[-1],
             MoreArgs = list(length.out = between_points))[-1,]
    )
  }
  
  new_value <- function(value, between_points) {
    c(value[1],
      mapply(seq, lag(value)[-1], value[-1],
             MoreArgs = list(length.out = between_points))[-1,]
    )
  }
  
  df <- data.frame(seq, position, value) %>%
    group_by(seq) %>%
    do(
      data.frame(seq = new_seq(.$seq, between_points),
                 position = new_position(.$position, between_points),
                 value = new_value(.$value, between_points)
      )
    )
  
  fit_points(df$seq, df$position, df$value, outer, inner, seq_df, max_value, min_value)
  
}


fit_link <- function(name_from, name_to, pos_from, pos_to, start_r, end_r, inner_r = 0.1) {
  
  theta <- c(pos_from, 
             mean(c(pos_from, pos_to)) + ifelse(abs(pos_from - pos_to) > pi, pi, 0),
             pos_to)
  
  ## altering the mid radius depending on the distance between points (far away pass through middle, close don't move too far away from start)
  ## this is just manually scaled to look okay on a circle of about radius 1 so needs more work
  r_inner <- pmin(abs(pos_from - pos_to), abs(abs(pos_from - pos_to) - 2*pi)) / 10
  
  r <- do.call(c, lapply(r_inner, function(x) c(start_r, inner_r + pi/10 - x, end_r)))
  
  data.frame(name_from = name_from, 
             name_to = name_to,
             theta,
             r = r)
  
}


fit_links <- function(name_from, name_to, pos_from, pos_to, seq_df, start_r, end_r, inner_r = 0.1) {
  
  link_df <- data.frame(seq_from = factor(name_from, levels = levels(seq_df$seq)),
                        seq_to = factor(name_to, levels = levels(seq_df$seq)), pos_from, pos_to)
  
  link_df %>%
    inner_join(seq_df, by = c("seq_from" = "seq")) %>%
    inner_join(seq_df, by = c("seq_to" = "seq")) %>%
    mutate(new_pos_from = seq_start.x + (pos_from * scale.x / length.x) * (seq_end.x - seq_start.x),
           new_pos_to = seq_start.y + (pos_to * scale.y / length.y) * (seq_end.y - seq_start.y)) %>%
    select(seq_from, seq_to, new_pos_from, new_pos_to) %>%
    group_by(link = row.names(.)) %>%
    do(fit_link(.$seq_from, .$seq_to, .$new_pos_from, .$new_pos_to, start_r, end_r, inner_r)) %>%
    mutate(inter = ifelse(name_to == name_from, "No", "Yes"))
  
}


# fit_points <- function(..., data = NULL) {
#   
#   UseMethod("fit_points", data)
#   
# }


fit_points <- function(seq, position, value, outer, inner, seq_df, metadata = NULL, max_value = NULL, min_value = NULL) {
  
  r <- fit_to_track(value, outer, inner, max_value, min_value)
  
  df <- fit_to_seq(seq, position, seq_df, metadata)
  
  data.frame(df, r, orig_value = value)
  
}



# fit_points.data.frame <- function(data, seq, position, value, outer, inner, seq_df, max_value = NULL, min_value = NULL) {
#   
#   sub_value <- eval(substitute(value), data, parent.frame())
#   
#   if(is.null(max_value)) max_value <- max(sub_value)
#   if(is.null(min_value)) min_value <- min(sub_value)
#   
#   fit_points(
#     seq = eval(substitute(seq), data, parent.frame()),
#     position = eval(substitute(position), data, parent.frame()),
#     value = sub_value,
#     outer, inner, seq_df, max_value, min_value
#   )
#    
# }



fit_ribbon <- function(name_from, name_to, pos_from_start, pos_from_end,
                       pos_to_start, pos_to_end,
                       start_r, end_r, inner_r = 0.1) {
  
  t <- c(0, 0.02, 0.25, 0.5, 0.75, 0.98, 1)
  
  from_mid <- mean(c(pos_from_start, pos_from_end))
  to_mid <- mean(c(pos_to_start, pos_to_end))
  
  theta_mid <-  mean(c(from_mid, to_mid)) + 
    ifelse(abs(from_mid - to_mid) > pi, pi, 0)
  
  theta <- c((pos_from_start * (1 - t) + pos_from_end * t),
             theta_mid,
             (pos_to_start * (1 - t) + pos_to_end * t),
             theta_mid,
             pos_from_start
  )
  
  r_inner <- pmin(abs(from_mid - to_mid), abs(abs(from_mid - to_mid) - 2*pi)) / 10
  
  r <- do.call(c,
               lapply(r_inner, function(x) c(rep(start_r, length(t)),
                                             inner_r + pi/10 - x,
                                             rep(end_r, length(t)),
                                             inner_r + pi/10 - x,
                                             start_r)
               )
  )
  
  data.frame(name_from = name_from,
             name_to = name_to,
             theta,
             r
  )
  
  
}


fit_ribbons <- function(name_from, name_to, pos_from_start, pos_from_end,
                        pos_to_start, pos_to_end, seq_df,
                        start_r, end_r, inner_r = 0.1, metadata = NULL) {
  
  ribbon_df <- data.frame(name_from = factor(name_from, levels = levels(seq_df$seq)),
                          name_to = factor(name_to, levels = levels(seq_df$seq)),
                          pos_from_start = pos_from_start,
                          pos_from_end = pos_from_end,
                          pos_to_start = pos_to_start,
                          pos_to_end = pos_to_end
  )
  
  transformed <- ribbon_df %>%
    inner_join(seq_df, by = c("name_from" = "seq")) %>%
    inner_join(seq_df, by = c("name_to" = "seq")) %>%
    mutate(new_pos_from_start = seq_start.x + (pos_from_start * scale.x / length.x) * (seq_end.x - seq_start.x),
           new_pos_from_end = seq_start.x + (pos_from_end * scale.x / length.x) * (seq_end.x - seq_start.x),
           new_pos_to_start = seq_start.y + (pos_to_start * scale.y / length.y) * (seq_end.y - seq_start.y),
           new_pos_to_end = seq_start.y + (pos_to_end * scale.y / length.y) * (seq_end.y - seq_start.y)) %>%
    select(name_from, name_to, new_pos_from_start, new_pos_from_end,
           new_pos_to_start, new_pos_to_end) %>%
    group_by(link = row.names(.)) %>%
    do(fit_ribbon(.$name_from, .$name_to, .$new_pos_from_start, .$new_pos_from_end,
                  .$new_pos_to_start, .$new_pos_to_end, start_r, end_r, inner_r)) %>%
    mutate(inter = ifelse(name_to == name_from, "No", "Yes"))
  
  if (is.null(metadata)) {
    return(transformed)
  } else {
    metadata$link <- row.names(metadata)
    transformed %>%
      inner_join(metadata, "link")
  }
  
}

## S3 method moved to fit_points() which calls this function
# fit_to_seq <- function(..., data = NULL) {
#   
#   UseMethod("fit_to_seq", data)
#   
# }

fit_to_seq <- function(seq, positions, seq_df, metadata = NULL) {
  
  if (is.null(metadata)) {
    
    df <- data.frame(seq = seq, position = positions)
    
  } else {
    
    df <- data.frame(seq = seq, position = positions, metadata)
    
  }
  
  df %>% 
    inner_join(seq_df, by = "seq") %>% 
    mutate(theta = seq_start + (position * scale / length) * (seq_end - seq_start)) %>%
    select(-seq_start, -seq_end, -length)
  
}


# fit_to_seq.data.frame <- function(data, seq, positions, seq_df) {
#   
#   sub_seq <- substitute(seq)
#   
#   fit_to_seq(seq = eval(sub_seq, data, parent.frame()),
#              positions = eval(substitute(positions), data, parent.frame()),
#              seq_df) %>%
#     cbind(data)
#   
# }



fit_to_track <- function(values, outer, inner, max_value = max(values), min_value = min(values)) {
  
  if(is.null(max_value)) max_value <- max(values)
  if(is.null(min_value)) min_value <- min(values)
  
  stand <- (values - min_value) / (max_value - min_value)
  
  scaled <- stand * (outer - inner) + inner
  
  scaled
  
}
