Broman’s Socks: Exploring The Impact of Bayesian Priors pandoc\_args:
–webtex
================

# Introduction

In this analysis we explore the problem of *Broman’s Socks*, first
analysed by Rasmus Baath in
[this](http://www.sumsar.net/blog/2014/10/tiny-data-and-the-socks-of-karl-broman/)
blog post. The problem is as follows: given that the first 11 socks that
Karl Broman removed from his washing machine were all distinct, how many
socks do we believe were in the washing machine to begin with?

Baath’s analysis procedes using Approximate Bayesian Computation, and is
an excellent introduction to this methodology. This allows Baath to
avoid computation of the likelihood function (which requires some
experience in probability/combinatorics), at the expense of
computational efficiency.

In this note we will start by calculating the likelihood explicitly,
enabling us to forego any monte carlo techniques as the problem becomes
exactly solveable. We also propose a number of different prior
specifications for the problem, and explore the impact that these have
on the posterior distribution.

# Defining the likelihood

We start by determining the formula for the likelihood. The parameters
for our model will be \(p\), the number of pairs of socks in the washing
machine, and \(s\) the number of singleton socks. The total number of
socks is therefore \(n = 2p + s\).

For full generality we let \(k\) be the number of distinct socks that
have been removed from the washing machine; in the case of Karl Broman’s
original tweet \(k=11\). The likelihood of the parameters \((p,s)\)
given \(k\) is then given by:

\[ L(p,s | k) = \mathbf P[k |p,s],\]

where the right hand side is the probability that you pick \(k\) socks
out of \(2p + s\) and they are all distinct. This can be found by
solving two combinatorics (eg. difficult counting) problems: counting
the total number of ways to pick \(k\) distinct socks, and dividing by
the total number of ways to pick \(k\) socks.

This gives the following formula for the likelihood:

\[L(p,s|k) = 
\begin{cases}
\binom{2p + s}{k}^{-1} \sum_{j=0}^s 2^{k-j} \binom{s}{j} \binom{p}{k-j} & \text{if } k \leq p + s, \\
0 & \text{else.}
\end{cases}
\]

For the purpose of computation, it will be more efficient to work with
the log-likelihood, which we define in R below

``` r
log_likelihood <- function(p,s,k){
  if(k > p + s){
    return(-Inf)
  } else{
    
    numerator_df <- data_frame(
      j = 0:s,
      log_n_term = lchoose(s,j) + lchoose(p,k-j) + (k-j)*log(2)
    ) %>% mutate(
      #log_n_term = log_n_term - max(log_n_term),
      n_term = exp(log_n_term)
    )
    
    log_numerator <- log(sum(numerator_df$n_term))
    
    log_denominator <- lchoose(2*p + s, k)
    
    return(log_numerator - log_denominator)
  }
}


log_likelihood <- Vectorize(log_likelihood)
```

We vectorize the likelihood function to facilitate using it in
conjunction with `dplyr` commands later on.

# Introducing the machinery

Before we dive into defining specific prior distributions, and exploring
the results, we first run through the actual computation approach we
will use so that we can separate the mechanics from the art in later
sections.

We will define a data frame which has a row enumerating every
combination of pairs and single socks \((p,s)\) for values of
\(0 \leq p,s \leq 50\), which means that the data frame will have
\(51^2 = 2601\) rows. We are already building in some prior belief here:
that the washing machine does not contain more than 150 socks (50 pairs,
and 50 singletons). We can initiate this data frame using the `crossing`
function from the `dplyr` package.

``` r
library(tidyverse)

socks <- crossing(data_frame(p = 0:50), data_frame(s = 0:50))
```

To this we add columns for the total number of socks \(n\), and the
observed number removed from the machine, \(k = 11\), for our purposes.

``` r
socks <- socks %>% mutate(n = 2*p + s, k = 11)
```

With this we can already calculate the log-likelihood using the function
we defined in the previous section

``` r
socks <- socks %>% mutate(log_likelihood = log_likelihood(p,s,k))
```

The art will come when we define the prior distribution on \((p,s)\),
but having done so it will be easy to add a column to the data frame
denoting the logarithm of this (since we are working with
log-likelihoods, we also want to use log-priors). For illustrative
purposes only we consider a flat prior on \((p,s)\) which is equivalent
to assuming that the log-prior is a constant which we will take to be
\(1\).

``` r
socks <- socks %>% mutate(log_prior = 1)
```

Bayes theorem asserts that the posterior distribution is then
proportional to the prior times the likelihood, or equivalently that the
log of the posterior is equal to the sum of the log-prior and
log-likelihood, plus a constant. We therefore define
`log_posterior_tilde` to be this formula, excluding the (unknown)
constant.

``` r
socks <- socks %>% mutate(log_posterior_tilde = log_prior + log_likelihood)
```

Taking the exponential of this then returns the posterior distribution,
up to some normalising constant. If we denote this un-normalised
posterior by \(\tilde P(p,s|k)\), then defining
\(Z = \sum_{p,s} \tilde P(p,s|k)\) for the sum of the un-normalised
values one can confirm that the posterior probability is given by the
formula

\[ P(p,s|k) = Z^{-1} \tilde P(p,s|k).\] There are two computation issues
with this. The first is that in defining our data frame we have
truncated the total domain on which the posterior distribution lives: in
that we have focused only on scenarios where up to 50 pairs and 50
single socks are included. This issue will have little impact so long as
our choice of priors do not put significant weight on such extreme
scenarios.

The second issue is computational: throughout we have described
calculations on the log scale as these are less likely to run into
computational *underflow* problems: i.e. working with very small numbers
which require on calculations at the same scale of the computer’s
precision. In the final step of calculating \(Z\) we risk undoing all
this good work.

For this reason, the implementation we use requires a small trick:
before taking the exponential of \(\log \tilde P(p,s|k)\) we first add
on a constant to this. Adding a constant now will equate to multiplying
by a constant on the natural scale, which will later be absorbed
calculting \(Z\). The advantage is that by adding this constant we can
ensure that the summation required to compute \(Z\) does not involve any
numbers at the level of the machine precision. The constant we use is
the negative of the maximum value of `log_posterior_tilde`, which will
mean that on the natural scale the largest value of \(\tilde P(p,s|k)\)
will be \(1\).

``` r
socks <- socks %>% 
  mutate(
    log_posterior_tilde = log_posterior_tilde - max(log_posterior_tilde),
    posterior_tilde = exp(log_posterior_tilde),
    posterior = posterior_tilde / sum(posterior_tilde),
    log_posterior = log(posterior)
)
```

And we are done\! We now have an expression for the posterior and
log-posterior (useful for plotting). In this particular run through of
the steps we will not pay close attention to the outcome of the
analysis: this is due to the choice of the flat prior
\(P(p,s) \propto 1\). Whilst convenient for the purpose of exposition,
this particular choice of prior is *improper*: meaning that it is not
itself a probability distribution. Though in some contexts it is
possible to use an improper prior and obtain a proper posterior, this is
not one of them. Directly calculating the resulting formula for the
normalising constant on all pairs \((p,s)\) (i.e. not constrained to
values \(0 \leq p,s \leq 50\)) would show that the normalising constant
is \(Z = \infty\).

Before moving along to defining priors we wrap all of the steps in the
code above into a single function; we include in this function the
ability to provide custom functions for the log likelhood (which
otherwise defaults to the previously defined function), and the log
prior (which otherwise defaults to the improper, constant,
prior).

``` r
socks_bayes <- function(p_max = 50, s_max = 50, k = 11, log_likelihood = NULL, log_prior = NULL){

  # if(is.null(log_likelihood)){
  #   log_likelihood <- function(p,s,k){
  #     if(k > p + s){
  #       return(-Inf)
  #     } else{ 
  #       return(lchoose(p+s,k) - lchoose(2*p + s, k))
  #     }
  #   }
  #   
  #   log_likelihood <- Vectorize(log_likelihood)
  # }
  
  if(is.null(log_prior)){
    
    warning("Warning: Defaulting to the constant prior; this may yield an improper posterior.")
    
    log_prior <- function(p,s){1}
    log_prior <- Vectorize(log_prior)
    }
  
  socks <- crossing(data_frame(p = 0:p_max), data_frame(s = 0:s_max)) %>%
    mutate(
      n = 2*p + s,
      k = k,
      
      log_likelihood = log_likelihood(p,s,k),
      log_prior = log_prior(p,s),
      prior = exp(log_prior),

      log_posterior_tilde = log_prior + log_likelihood,

      log_posterior_tilde = log_posterior_tilde - max(log_posterior_tilde),

      posterior_tilde = exp(log_posterior_tilde),
      posterior = posterior_tilde / sum(posterior_tilde),

      log_posterior = log(posterior)
  )

  socks <- socks %>% select(p,s,n,k, log_prior, log_likelihood, prior, log_posterior, posterior)

  return(socks)
}
```

# Baath’s Prior

Our first analysis will replicate that conducted by Baath, who
constructs the prior on \((p,s)\) as follows.

## Defining the Prior

First a prior is placed on \(n\), the overall total number of socks that
are believed to be in the washing machine. He chooses to use a negative
binomial distribution. Baath chooses parameters for this distribution
based on prior knowledge that Broman is one in a family of four, and a
belief that Broman only runs one wash per week, and as such decided to
use a negative binomial distribution with mean \(\mu = 30\)
(corresponding to 15 pairs of socks), with a standard deviation of
\(\sigma = 15\); we denote this distribution by \(P_{\mu,\sigma}(n)\).

``` r
prior_n <- function(n, mu = 30, sigma = 15){

  size <- -mu^2 / (mu - sigma^2)
  
  prior_prob <- dnbinom(n, mu = mu, size = size)

  return(prior_prob)
}
```

![](broman-socks_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Having determined the total number of socks that he expects there to be
in any given wash, Baath then uses a prior for the proportion of socks
that are pairs, as opposed to singletons. Denoting this proportion
\(\theta = 2p/(2p + s)\), Baath places on this a Beta prior (the natural
choice for a proportion measure between 0 and 1). This has prior
parameters \(\alpha\), \(\beta\) which are chosen to be
\(\alpha = 15, \, \beta = 2\), which were chosen to conform with his own
laundry habits. We will denote this distribution by
\(P_{\alpha,\beta}(\theta)\).

![](broman-socks_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

We now have to work out how to turn priors on \(n\) and \(\theta\) into
priors for \(p,s\); this is handled fairly easily in Baath’s original
computation approach through a sampling process:

1.  Sample \(n\), and \(\theta\) from the respective prior distributions
    defined above.
2.  Let \(p = \left[ \theta \rfloor n/2 \lfloor \right]\), where
    \(\rfloor x \lfloor\) denotes the floor of \(x\), and
    \(\left[x\right]\) denotes \(x\) rounded to the nearest integer.
3.  Let \(s = n - 2p\).

Without providing full details, following these steps mathematically
leads to the following formula for the prior distribution of \((p,s)\)

\[
P(p,s) = P_{\mu,\sigma}(2p+s) \left\{ F_{\alpha,\beta}\left(\frac{2p +1}{2\lfloor p + s/2 \rfloor}\right) - F_{\alpha,\beta}\left(\frac{2p -1}{2\lfloor p + s/2 \rfloor}\right) \right\},
\] where \(F_{\alpha,\beta}(t) = P_{\alpha, \beta}(\theta \leq t)\) is
the cummulative density of the Beta distribution; the term in braces
corresponds to the probability that \(\theta\) lies in the range of
values that when multiplied by \(2p + s\), and rounded to the nearest
integer returns the answer of \(p\).

We define the prior
below

``` r
log_prior_baath <- function(p,s, mu = 30, sigma = 15, alpha = 15, beta = 2){
  
  if(min(alpha,beta) == 0 & s > 0){
    return(-Inf)
  }
  
  n <- 2*p + s
  
  prior_n <- prior_n(n, mu, sigma)
  
  theta_hgh <- (2*p + 1)/(2 * floor(n/2) ) 
  theta_low <- (2*p - 1)/(2 * floor(n/2) )
  
  theta_hgh <- (theta_hgh %>% max(.,0)) %>% min(.,1)
  theta_low <- (theta_low %>% max(.,0)) %>% min(.,1)
  
  prior_theta <-pbeta(theta_hgh, shape1 = alpha, shape2 = beta) - pbeta(theta_low, shape1 = alpha, shape2 = beta)
  
  return( log(prior_n) + log(prior_theta))
}

log_prior_baath <- Vectorize(log_prior_baath)
```

The 2D density plot below shows how the prior distribution varies over
combinations of (2p,s); the highest density goes to the scenario in
which there are a total of 19 socks, made up of \(p = 8\) pairs and
\(s = 3\) singletons.
![](broman-socks_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

## Posterior Analysis

Having defined the prior distribution, and already having described the
processing steps to derive the posterior, we can now immediately obtain
the posterior
distribution:

``` r
socks_baath <- socks_bayes(p_max = 50, s_max = 50, k = 11, log_likelihood = log_likelihood, log_prior = log_prior_baath)
```

``` r
posterior_mode <- socks_baath %>% slice(which.max(log_posterior))
```

As with the prior, we can now plot the posterior density as a function
of \((2p,s)\), and find that the single most likely scenario in the
posterior is that there was a total of \(n = 37\) socks, made up of
\(p = 16\) pairs and \(s = 5\) singletons.

![](broman-socks_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Alternatively we can calculate the density of total socks - disregarding
the breakdown into pairs and singletons.

``` r
total_socks_baath <- socks_baath %>% group_by(n) %>%
  summarise(
    prior = sum(prior),
    posterior = sum(posterior)
  ) %>% ungroup() %>% arrange(n) %>%
  mutate(
    prior_accum = cumsum(prior),
    posterior_accum = cumsum(posterior)
  )
```

![](broman-socks_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

# A Factored Prior

Baath’s prior assumes that singleton socks make up a certain proportion
of the total number of socks, and then puts a prior on this proportion,
and the total number.

An alternative approach is to assume that the number of pairs of socks,
and the number of singleton socks are independently distributed. One
advantage of this model is the relative simplicity in defining the
prior, which we no longer have to re-parameterise from \((n,\theta)\) to
\((p,s)\). Whether or not this independence assumption is more
reflective of actual washing practice remains open to debate.

Like Baath, we will use negative binomial distributions to model the
number of socks, but now use separate distributions for each of \(p\),
and \(s\). Following Baath (for reasons that will become clear below) we
will parameterise the distributions in terms of the mean and standard
deviation of the
distributions:

\[ p \sim \text{NBin}(\mu_p, \sigma_p), \qquad s \sim \text{NBin}(\mu_s, \sigma_s).\]
In the natural parameterisation of the negative binomial, this equates
to parameters

\[\mu = \frac{qr}{(1-q)}, \qquad \sigma^2 = \frac{\mu}{(1-q)}\] Or:
\[ q = 1 - \mu/\sigma^2, \qquad r = \mu  \frac{\mu/\sigma^2}{1 - \mu/\sigma^2} = \frac{\mu}{\sigma^2/\mu - 1} = \frac{\mu^2}{\sigma^2 - \mu}\]
To closely match the prior that Baath constructed, we want that
\(\mathbf E[ 2p + s] = 2\mu_p = \mu_s = 30\), and moreover that
\(\mathbf E[(2p)/(s+2p)] = 15/17\)

\[ \text{Var}(2p + s) = 4 \text{Var}(p) + \text{Var}(s) = 15^2\].

where we choose to parameterise the distributions via their mean and ,
which is the parameterisation used by Baath. Our intent will be to
choose the parameters to closely match those used by Baath for the
distribution We will make use of the particular property of negative
binomial distributions

For our purposes we will assume that the number of pairs and singletons
is Poisson distributed with respective means \(\mu_p = 13.25\) and
\(\mu_s = 3.5\); note that the parameters have been chosen so that the
summary statistics for the prior are close to those of Baath. p2 =
rnbinom(n = n, mu = 13, size =
5.5),

``` r
log_prior_factored <- function(p,s, mu_p = 13, mu_s = 4, size_p = 5.5, size_s = 3){

  log_prior_p <- dnbinom(p, mu = mu_p, size = size_p, log = TRUE)
  log_prior_s <- dnbinom(s, mu = mu_s, size = size_s, log = TRUE)
  
  return( log_prior_p + log_prior_s)
}


log_prior_factored <- Vectorize(log_prior_factored)
```

The 2D density plot below shows how the prior distribution varies over
combinations of (2p,s); the highest density goes to the scenario in
which there are a total of 22 socks, made up of \(p = 10\) pairs and
\(s = 2\) singletons.

![](broman-socks_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

## Posterior Analysis

Having defined the prior distribution, and already having described the
processing steps to derive the posterior, we can now immediately obtain
the posterior
distribution:

``` r
socks_factored <- socks_bayes(p_max = 50, s_max = 50, k = 11, log_likelihood = log_likelihood, log_prior = log_prior_factored)
```

``` r
posterior_mode <- socks_factored %>% slice(which.max(log_posterior))
```

As with the prior, we can now plot the posterior density as a function
of \((2p,s)\), and find that the single most likely scenario in the
posterior is that there was a total of \(n = 37\) socks, made up of
\(p = 17\) pairs and \(s = 3\) singletons.

![](broman-socks_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

Alternatively we can calculate the density of total socks - disregarding
the breakdown into pairs and singletons.

``` r
total_socks_factored <- socks_factored %>% group_by(n) %>%
  summarise(
    prior = sum(prior),
    posterior = sum(posterior)
  ) %>% ungroup() %>% arrange(n) %>%
  mutate(
    prior_accum = cumsum(prior),
    posterior_accum = cumsum(posterior)
  )
```

![](broman-socks_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

# Was the 11th Sock a Stopping Time?

The analysis so far, and that of Baath, has supposed that Broman chose
to stop at the 11th sock by free will, before continuing to unload his
washing. An alternative hypothesis might be that Broman saw that the
12th sock was going to break this streak, and so decided to tweet at
this point.

This is not an update to our prior beliefs of the parameters, but rather
to the probabilistic model itself, i.e.a change in the likelihood.

The revised likelihood can be expressed as

\[L(p,s|k) = 
\frac{ \sum_{j=0}^s 2^{k-j} \binom{s}{j} \binom{p}{k-j} }{ \binom{2p + s}{k}}
\]

\[\tilde L(p,s|k) = 
\frac{ \binom{k}{1} \sum_{j=0}^s 2^{k-j} \binom{s}{j} \binom{p}{k-j} }{ \binom{2p + s}{k + 1}} = k \frac{\binom{2p+s}{k}}{\binom{2p+s}{k+1}}  L(p,s|k) = \frac{ k (k+1) }{2p+s - k} L(p,s|k)
\]

``` r
log_likelihood_stopped <- function(p,s,k){
  
  if(k + 1> 2*p + s){ return(-Inf)}
  
  log(k) + log(k+1) - log(2*p + s -k) + log_likelihood(p,s,k)

}

log_likelihood_stopped <- Vectorize(log_likelihood_stopped)
```

``` r
socks_stopped <- socks_bayes(p_max = 50, s_max = 50, k = 11, log_likelihood = log_likelihood_stopped, log_prior = log_prior_baath)
```

``` r
posterior_mode <- socks_stopped %>% slice(which.max(log_posterior))
```

As with the prior, we can now plot the posterior density as a function
of \((2p,s)\), and find that the single most likely scenario in the
posterior is that there was a total of \(n = 31\) socks, made up of
\(p = 14\) pairs and \(s = 3\) singletons.

![](broman-socks_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
total_socks_stopped <- socks_stopped %>% group_by(n) %>%
  summarise(
    prior = sum(prior),
    posterior = sum(posterior)
  ) %>% ungroup() %>% arrange(n) %>%
  mutate(
    prior_accum = cumsum(prior),
    posterior_accum = cumsum(posterior)
  )
```

![](broman-socks_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->
