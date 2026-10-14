

library(Matrix)
library(ggplot2)

# Parameters (Buyer of the HDD Put)
K        <- 1150           # Strike (HDD)
cap_rng  <- 300            # Hedged range -> Cap-Level = 850
rate     <- 100000         # USD per HDD
premium  <- 3330000        # Premium paid (USD)

S_cap    <- K - cap_rng    # 850

# HDD range
S <- 0:2000

# Payoff and Profit (Buyer)
payoff_usd <- rate * pmin(pmax(K - S, 0), cap_rng)
profit_usd <- payoff_usd - premium

# Key profit levels
max_profit <- max(profit_usd)   # +26.67 Mio
min_profit <- min(profit_usd)   # -3.33 Mio

df <- data.frame(S = S, profit_usd = profit_usd)

# Plot
ggplot(df, aes(x = S, y = profit_usd)) +
  geom_line(linewidth = 1.2, color = "#0072B2") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +                  # Break-even line
  geom_hline(yintercept = c(max_profit, min_profit), linetype = "dotted", color = "gray40") +  # Profit bounds
  geom_vline(xintercept = c(S_cap, K), linetype = "dashed", color = "gray40") +   # Cap & Strike
  geom_text(aes(x = K, y = 5e6, label = "1150 (Strike)"), 
            angle = 90, vjust = -0.5, hjust = -0.2, size = 3.5) +
  geom_text(aes(x = S_cap, y = 5e6, label = "850 (Cap)"), 
            angle = 90, vjust = -0.5, hjust = -0.2, size = 3.5) +
  geom_text(aes(x = 1900, y = max_profit, label = "+26.67 Mio"), 
            vjust = -0.5, hjust = 1, size = 3.5) +
  geom_text(aes(x = 1900, y = min_profit, label = "-3.33 Mio"), 
            vjust = 1.5, hjust = 1, size = 3.5) +
  labs(
    title = "HDD Put – Profit of the Buyer (Energy Company, London)",
    x = "Cumulative HDD (Nov–Jan)",
    y = "Profit (USD)"
  ) +
  scale_x_continuous(
    limits = c(0, 2000),
    breaks = c(0, 500, 850, 1000, 1150, 1500, 2000),
    labels = c("0", "500", "850", "1000", "1150", "1500", "2000")
  ) +
  theme_minimal(base_size = 13)


#----------------------------------------------------------------------------------------------------------
# Gegebene HDD-Kennzahlen (Originalebene)
mean_x <- 1191.334375
sd_x   <- 84.148776

# Umwandlung in Lognormalparameter (Normalebene)
sigma2 <- log(1 + (sd_x / mean_x)^2)
sigma  <- sqrt(sigma2)
mu     <- log(mean_x) - 0.5 * sigma2

# X-Achse (Index-Werte)
x <- seq(0, 2000, by = 1)

# Dichte des Index (Lognormalverteilung)
f_x <- dlnorm(x, meanlog = mu, sdlog = sigma)

# Werte für Cap und Strike
x_cap    <- 850
x_strike <- 1150
f_cap    <- dlnorm(x_cap, meanlog = mu, sdlog = sigma)
f_strike <- dlnorm(x_strike, meanlog = mu, sdlog = sigma)

# Dataframe
df <- data.frame(x = x, density = f_x)

# Plot
ggplot(df, aes(x = x, y = density)) +
  geom_line(color = "#0072B2", linewidth = 1.2) +
  geom_vline(xintercept = c(x_cap, x_strike), linetype = "dotted", color = "gray50") +
  geom_point(aes(x = x_cap, y = f_cap), color = "black") +
  geom_point(aes(x = x_strike, y = f_strike), color = "black") +
  annotate("text", x = x_cap,    y = f_cap,    label = sprintf("f(850)=%.5f", f_cap),    vjust = -1, hjust = 0.5) +
  annotate("text", x = x_strike, y = f_strike, label = sprintf("f(1150)=%.5f", f_strike), vjust = -1, hjust = 0.5) +
  labs(
    title = "Density Function of HDD Index X (Lognormal Model)",
    x = "Cumulative HDD (Index)",
    y = "Density f_X(x)"
  ) +
  scale_x_continuous(limits = c(0, 2000),
                     breaks = c(0, 500, 850, 1000, 1150, 1500, 2000)) +
  theme_minimal(base_size = 13)
#----------------------------------------------------------------------------------------------------
#Nephila pays the energy company when the HDD index is low, that is,
#when the winter has many warm days.
#-----------------------------------------------------------------------------------------------------



#2.) 

mean_x <- 1191.334375; sd_x <- 84.148776
K <- 1150; cap_rng <- 300; rate <- 100000
alpha <- 0.995
n <- 2e6  

# Lognormal-Parameter
sigma2 <- log(1 + (sd_x/mean_x)^2); sigma <- sqrt(sigma2)
mu <- log(mean_x) - 0.5*sigma2

set.seed(1)
S <- rlnorm(n, meanlog = mu, sdlog = sigma)

# Payout (USD)
payout <- rate * pmin(pmax(K - S, 0), cap_rng)

# Kennzahlen
EP  <- mean(payout)
VaR <- as.numeric(quantile(payout, probs = alpha, type = 7))
TVaR <- mean(payout[payout >= VaR])  # Expected Shortfall (α)

# Preis mit tail-basiertem Risk Capital
ROC <- 0.09
offer_price <- EP + ROC * (TVaR - EP)

list(EP = EP, VaR = VaR, TVaR = TVaR, Offer = offer_price)

#--------------------------------------------------------------------------------------------------------------------


# --- Inputs ---
mean_x <- 1191.334375; sd_x <- 84.148776
K <- 1150; U <- 300; c_rate <- 100000
alpha <- 0.995

# --- Lognormal parameters ---
sigma2 <- log(1 + (sd_x/mean_x)^2)
sigma  <- sqrt(sigma2)
mu     <- log(mean_x) - 0.5*sigma2
Phi <- pnorm; FS <- function(s) plnorm(s, mu, sigma)
m1  <- exp(mu + 0.5*sigma^2)

# --- Helper functions ---
E_put_uncapped <- function(K_) {
  if (K_ <= 0) return(0)
  a <- (log(K_) - mu)/sigma
  K_*Phi(a) - m1*Phi(a - sigma)
}
E_S_leq <- function(s) m1 * Phi((log(s) - mu - sigma^2)/sigma)

# --- Expected payout (capped) ---
E_pay <- c_rate * (E_put_uncapped(K) - E_put_uncapped(K - U))

# --- VaR_alpha of payout ---
VaR_pay <- function(alpha_) {
  s_q <- qlnorm(1 - alpha_, mu, sigma)
  c_rate * max(0, min(K - s_q, U))
}
VaR_a <- VaR_pay(alpha)

# --- TVaR_alpha of payout ---
TVaR_pay <- function(alpha_) {
  y_a <- VaR_pay(alpha_)
  if (y_a >= c_rate*U) return(c_rate*U)
  s_a <- K - y_a/c_rate
  s_min <- K - U
  tailp <- 1 - alpha_
  if (s_a <= s_min) return(c_rate*U)
  part <- K*(FS(s_a)-FS(s_min)) - (E_S_leq(s_a)-E_S_leq(s_min))
  (c_rate * (U*FS(s_min) + part)) / tailp
}
TVaR_a <- TVaR_pay(alpha)

# --- Offer price with ROC = 9% ---
ROC <- 0.09
Offer <- E_pay + ROC * (TVaR_a - E_pay)

list(Expected_Payout = E_pay, TVaR_0.995 = TVaR_a, Offer_Price = Offer)
#-------------------------------------------------------------------------------------------------------------------------------
# algebra and text are in notepad 

#---------------------------------------------------------------------------------------------------------------------------------
# -------- Setup --------


# Contracts (means/sds on original index scale)
contracts <- data.frame(
  name   = c("HDD Put", "Frost Day Call", "Snow Call", "Rainfall Put", "Dry Days Put"),
  type   = c("put",     "call",           "call",      "put",          "put"),  # <-- fixed: Dry Days Put is PUT
  mean_x = c(1191.334375, 10.25, 36.225231, 52.565122, 71.625),
  sd_x   = c(  84.148776,  5.983849, 13.655766, 19.652364, 10.165565),
  strike = c(1150, 16, 60, 35, 66),
  caprng = c(300, 15, 30, 15, 20)   # range in index units to reach the cap
)

# ---- Helper: one CDF plot with shaded P(cap reached) ----
plot_index_cdf_with_cap <- function(name, type, mean_x, sd_x, strike, caprng) {
  # lognormal params from mean/sd
  sigma2 <- log(1 + (sd_x/mean_x)^2)
  sigma  <- sqrt(sigma2)
  mu     <- log(mean_x) - 0.5*sigma2
  
  # cap level (index scale)
  cap_level <- if (type == "put") strike - caprng else strike + caprng
  
  # x-range
  x_max <- max(mean_x + 6*sd_x, strike + abs(caprng)*1.5, cap_level) * 1.2
  x <- seq(0, x_max, length.out = 3000)
  
  # CDF of index
  Fx <- plnorm(x, meanlog = mu, sdlog = sigma)
  
  # Probability that cap is reached
  if (type == "put") {
    p_cap <- plnorm(cap_level, meanlog = mu, sdlog = sigma)     # P(S <= cap)
    shade_df <- data.frame(x = x[x <= cap_level], y = Fx[x <= cap_level])
    subtitle <- "Shaded: P(S ≤ cap level)"
    label_x  <- cap_level * 0.75
    label_y  <- 0.15
  } else {
    p_cap <- 1 - plnorm(cap_level, meanlog = mu, sdlog = sigma) # P(S ≥ cap)
    shade_df <- data.frame(x = x[x >= cap_level], y = Fx[x >= cap_level])
    subtitle <- "Shaded: P(S ≥ cap level)"
    label_x  <- cap_level + (x_max - cap_level) * 0.5
    label_y  <- 0.85
  }
  
  # pretty label
  prob_label <- if (p_cap < 1e-8) {
    paste0("P(cap) ≈ ", format(p_cap*100, digits = 2, scientific = TRUE), "%")
  } else {
    paste0("P(cap) = ", signif(p_cap*100, 4), "%")
  }
  
  # strike/cap CDF values for markers
  F_strike <- plnorm(strike, meanlog = mu, sdlog = sigma)
  F_cap    <- plnorm(cap_level, meanlog = mu, sdlog = sigma)
  
  # plot
  ggplot(data.frame(x = x, Fx = Fx), aes(x = x, y = Fx)) +
    geom_line(linewidth = 1.1, color = "#0072B2") +
    geom_area(data = shade_df, aes(x = x, y = y),
              inherit.aes = FALSE, alpha = 0.25, fill = "#0072B2") +
    geom_vline(xintercept = c(strike, cap_level), linetype = "dashed", color = "gray50") +
    geom_point(aes(x = strike, y = F_strike), color = "black") +
    geom_point(aes(x = cap_level, y = F_cap),  color = "black") +
    annotate("text", x = strike,    y = F_strike, label = paste0("Strike = ", strike),
             vjust = -0.8, hjust = 0.5, size = 3.4) +
    annotate("text", x = cap_level, y = F_cap,    label = paste0("Cap level = ", cap_level),
             vjust = -0.8, hjust = 0.5, size = 3.4) +
    annotate("label", x = label_x, y = label_y, label = prob_label, size = 3.6, fill = "white") +
    labs(
      title = paste0(name, " — Index CDF (Lognormal)"),
      subtitle = subtitle, x = "Index value", y = "F(x) = P(S ≤ x)"
    ) +
    coord_cartesian(xlim = c(0, x_max), ylim = c(0, 1)) +
    theme_minimal(base_size = 13)
}

# ---- Generate & display all ----
plots <- lapply(1:nrow(contracts), function(i) {
  with(contracts[i, ], plot_index_cdf_with_cap(name, type, mean_x, sd_x, strike, caprng))
})
for (p in plots) print(p)

# Console summary: probability cap is reached
cap_probs <- sapply(1:nrow(contracts), function(i) {
  mean_x <- contracts$mean_x[i]; sd_x <- contracts$sd_x[i]
  sigma2 <- log(1 + (sd_x/mean_x)^2); sigma <- sqrt(sigma2)
  mu <- log(mean_x) - 0.5*sigma2
  strike <- contracts$strike[i]; caprng <- contracts$caprng[i]
  type <- contracts$type[i]
  cap_level <- if (type == "put") strike - caprng else strike + caprng
  if (type == "put") {
    plnorm(cap_level, meanlog = mu, sdlog = sigma)
  } else {
    1 - plnorm(cap_level, meanlog = mu, sdlog = sigma)
  }
})
cap_summary <- cbind(contracts[, c("name","type","strike","caprng")],
                     CapLevel = ifelse(contracts$type=="put",
                                       contracts$strike - contracts$caprng,
                                       contracts$strike + contracts$caprng),
                     P_cap = cap_probs)
print(cap_summary)

#---------------------------------------------------------------------------
## ===============================
## Monte Carlo VaR for Nephila
## ===============================


set.seed(1)

## 1) Contract & index inputs (means/sds are on the ORIGINAL index scale)
contracts <- data.frame(
  name    = c("HDD Put","Frost Day Call","Snow Call","Rainfall Put","Dry Days Put"),
  type    = c("put","call","call","put","put"),            # trigger direction
  mean_x  = c(1191.334375, 10.25, 36.225231, 52.565122, 71.625),
  sd_x    = c(  84.148776,  5.983849, 13.655766, 19.652364, 10.165565),
  strike  = c(1150, 16, 60, 35, 66),                       # index strike
  caprng  = c(300, 15, 30, 15, 20),                        # range in index units to cap
  maxPay  = c(30e6, 15e6, 15e6, 15e6, 15e6),               # USD max payout
  stringsAsFactors = FALSE
)

## Payout rate implied by cap: rate = maxPay / caprng (USD per index unit)
contracts$rate <- contracts$maxPay / contracts$caprng

## From Nephila’s P&L perspective:
##  +1 if Nephila pays (puts),  -1 if Nephila receives (calls)
contracts$sign_nephila <- +1L

## 2) Correlation matrix (your table)
Sigma_corr <- as.matrix(read.table(text="
1           0.505897314 0.162730339 0.298301026 0.10104819
0.505897314 1           0.085248955 0.461434846 -0.232804123
0.162730339 0.085248955 1           -0.091077212 0.107691492
0.298301026 0.461434846 -0.091077212 1           -0.518763707
0.10104819 -0.232804123 0.107691492 -0.518763707 1
", header=FALSE))
colnames(Sigma_corr) <- rownames(Sigma_corr) <- contracts$name

## 3) Convert mean/sd -> lognormal parameters (meanlog, sdlog) per index
sigma2 <- log(1 + (contracts$sd_x / contracts$mean_x)^2)
sdlog  <- sqrt(sigma2)
meanlog <- log(contracts$mean_x) - 0.5 * sigma2

## 4) Monte Carlo settings
N <- 2e7   # number of scenarios (increase for stability)

## 5) Build correlated standard normals via Cholesky
##    (chol returns upper-triangular U with Sigma = U'U; use %*% t(U))
U <- chol(Sigma_corr)                        # upper triangular
Z <- matrix(rnorm(N * nrow(contracts)), N)   # iid N(0,1)
Zc <- Z %*% U                                # correlated N(0, Sigma)

## 6) Transform to lognormal indices: S_i = exp(meanlog_i + sdlog_i * Zc_i)
S <- sweep(Zc, 2, sdlog, `*`)
S <- sweep(S, 2, meanlog, `+`)
S <- exp(S)
colnames(S) <- contracts$name

## 7) Price each capped contract per scenario
##    Put:   payoff = rate * pmin( pmax(strike - S, 0), caprng )
##    Call:  payoff = rate * pmin( pmax(S - strike, 0), caprng )
pay_mat <- matrix(0, nrow=N, ncol=nrow(contracts))
for (j in 1:nrow(contracts)) {
  K  <- contracts$strike[j]
  Uc <- contracts$caprng[j]
  rt <- contracts$rate[j]
  if (contracts$type[j]=="put") {
    intrinsic <- pmax(K - S[,j], 0)
  } else {
    intrinsic <- pmax(S[,j] - K, 0)
  }
  pay_mat[,j] <- rt * pmin(intrinsic, Uc)
}
colnames(pay_mat) <- contracts$name

## 8) Portfolio loss for Nephila: puts are losses (+), calls are gains (−)
loss_mat  <- sweep(pay_mat, 2, contracts$sign_nephila, `*`)  # effectively identical to pay_mat
port_loss <- rowSums(loss_mat)   # total loss (USD)
i_check <- 420
indices_i  <- S[i_check, ]
losses_i   <- loss_mat[i_check, ]
total_loss <- port_loss[i_check]

cat("\n--- Scenario", i_check, "---\n")
print(round(indices_i, 4))     # simulated index realizations
print(round(losses_i, 2))      # loss per contract (USD)
cat("Total portfolio loss (Nephila):", round(total_loss, 2), "USD\n")

## 9) Compute VaR and TVaR (Expected Shortfall)
alpha  <- 0.99
VaR_99  <- as.numeric(quantile(port_loss, probs = alpha,  type = 7))
TVaR_99 <- mean(port_loss[port_loss >= VaR_99])

alpha2 <- 0.995
VaR_995  <- as.numeric(quantile(port_loss, probs = alpha2, type = 7))
TVaR_995 <- mean(port_loss[port_loss >= VaR_995])

ELoss   <- mean(port_loss)           # expected portfolio loss (all legs contribute +)
comp_EL <- colMeans(loss_mat)        # component expected losses (all positive)

## sanity checks
max_loss_theory <- sum(contracts$maxPay)   # = 90e6
list(
  Expected_Portfolio_Loss_USD = ELoss,
  VaR_99_USD  = VaR_99,   TVaR_99_USD  = TVaR_99,
  VaR_995_USD = VaR_995,  TVaR_995_USD = TVaR_995,
  Component_Expected_Loss_USD = setNames(comp_EL, contracts$name),
  Theoretical_Max_Loss_USD    = max_loss_theory,
  Empirical_Max_Loss_USD      = max(port_loss),
  Gap_To_Bound_99             = max_loss_theory - VaR_99,
  Gap_To_Bound_995            = max_loss_theory - VaR_995
)



hist(port_loss)

