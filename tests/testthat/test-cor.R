test_that("emr_cor works", {
    # The warning is due to the bit iterator
    expect_warning(res <- emr_cor("track0", c(0, 10, 500, 1000), cor.exprs = c("track0", "track1", "track2", "track3"), iterator = 1, stime = 20, etime = 5000, keepref = F))
    expect_equal(
        res,
        structure(list(n = structure(c(
            495, 24390, 24841, 27, 1189, 1259,
            112, 5313, 5600, 0, 1, 0, 27, 1189, 1259, 27, 1189, 1259, 8,
            239, 285, 0, 0, 0, 112, 5313, 5600, 8, 239, 285, 112, 5313, 5600,
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
        ), .Dim = c(
            3L, 4L,
            4L
        ), .Dimnames = list(
            c("(0,10]", "(10,500]", "(500,1000]"),
            c("track0", "track1", "track2", "track3"), c(
                "track0", "track1",
                "track2", "track3"
            )
        )), e = structure(c(
            5.74949494949495,
            256.26360530272, 749.765065818606, 508.259259259259, 492.1173254836,
            492.817315329627, 492.165178571429, 497.97113997114, 502.038065476191,
            NaN, 999, NaN, 5.2962962962963, 258.585786375105, 741.456314535346,
            508.259259259259, 492.1173254836, 492.817315329627, 654.625,
            480.921548117155, 509.35730994152, NaN, NaN, NaN, 5.1875, 257.904730535165,
            751.079017857143, 659.375, 481.899581589958, 488.349707602339,
            492.165178571429, 497.97113997114, 502.038065476191, NaN, NaN,
            NaN, NaN, 372, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 999, NaN
        ), .Dim = c(3L, 4L, 4L), .Dimnames = list(c(
            "(0,10]", "(10,500]",
            "(500,1000]"
        ), c("track0", "track1", "track2", "track3"), c(
            "track0",
            "track1", "track2", "track3"
        ))), var = structure(c(
            8.13522701765125,
            19901.4288699502, 20496.5982417824, 112708.26611797, 83813.2147881371,
            82268.2780286588, 84584.1401267538, 78951.1860540845, 79729.9823084597,
            NaN, 0, NaN, 7.83813443072702, 20034.522918242, 20532.4436833196,
            112708.26611797, 83813.2147881371, 82268.2780286588, 93722.234375,
            78938.4531956043, 77028.7342204439, NaN, NaN, NaN, 7.38448660714285,
            19753.0776658919, 20293.5604972497, 72780.234375, 81440.6677404107,
            89483.3295564447, 84584.1401267538, 78951.1860540845, 79729.9823084597,
            NaN, NaN, NaN, NaN, 0, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
            0, NaN
        ), .Dim = c(3L, 4L, 4L), .Dimnames = list(c(
            "(0,10]", "(10,500]",
            "(500,1000]"
        ), c("track0", "track1", "track2", "track3"), c(
            "track0",
            "track1", "track2", "track3"
        ))), cov = structure(c(
            8.13522701765125,
            19901.4288699502, 20496.5982417824, -7.33607681755848, 1307.80322354975,
            -825.35587581963, 18.0047433035716, 68.2096226893627, -100.628320352582,
            NaN, 0, NaN, -7.33607681755848, 1307.80322354975, -825.35587581963,
            112708.26611797, 83813.2147881371, 82268.2780286588, 10116.890625,
            557.344283771934, -278.420275640441, NaN, NaN, NaN, 18.0047433035716,
            68.2096226893627, -100.628320352582, 10116.890625, 557.344283771934,
            -278.420275640441, 84584.1401267538, 78951.1860540845, 79729.9823084597,
            NaN, NaN, NaN, NaN, 0, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
            0, NaN
        ), .Dim = c(3L, 4L, 4L), .Dimnames = list(c(
            "(0,10]", "(10,500]",
            "(500,1000]"
        ), c("track0", "track1", "track2", "track3"), c(
            "track0",
            "track1", "track2", "track3"
        ))), cor = structure(c(
            1, 1, 1, -0.00780512085404247,
            0.0319151324754185, -0.0200818964669277, 0.0227814997446138,
            0.00172722506688777, -0.00250167140567675, NaN, NaN, NaN, -0.00780512085404247,
            0.0319151324754185, -0.0200818964669277, 1, 1, 1, 0.122495183608131,
            0.0069511807626388, -0.00335353938190529, NaN, NaN, NaN, 0.0227814997446138,
            0.00172722506688777, -0.00250167140567675, 0.122495183608131,
            0.0069511807626388, -0.00335353938190529, 1, 1, 1, NaN, NaN,
            NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN
        ), .Dim = c(3L, 4L, 4L), .Dimnames = list(c(
            "(0,10]", "(10,500]",
            "(500,1000]"
        ), c("track0", "track1", "track2", "track3"), c(
            "track0",
            "track1", "track2", "track3"
        )))), breaks = list(c(
            0, 10, 500,
            1000
        )))
    )
})

test_that("emr_cor works when dataframe = TRUE", {
    # The warning is due to the bit iterator
    expect_warning(res <- emr_cor("track0", c(0, 10, 500, 1000), cor.exprs = c("track0", "track1", "track2", "track3"), iterator = 1, stime = 20, etime = 5000, keepref = F, dataframe = TRUE))

    expect_warning(res_non_df <- emr_cor("track0", c(0, 10, 500, 1000), cor.exprs = c("track0", "track1", "track2", "track3"), iterator = 1, stime = 20, etime = 5000, keepref = F, dataframe = FALSE))

    expect_true(all(res_non_df$n == res$n))
    expect_true(all(res_non_df$e == res$e, na.rm = TRUE))
    expect_true(all(res_non_df$var == res$var, na.rm = TRUE))
    expect_true(all(res_non_df$cov == res$cov, na.rm = TRUE))
    expect_true(all(res_non_df$cor == res$cor, na.rm = TRUE))

    expect_true(is.factor(res$i))
    expect_true(is.factor(res$j))
    expect_equal(levels(res$i), c("track0", "track1", "track2", "track3"))
    expect_equal(levels(res$j), c("track0", "track1", "track2", "track3"))
    expect_equal(levels(res$track0), rownames(res_non_df$n))

    # make sure we have all the pairs
    pairs <- combn(levels(res$i), 2) %>%
        t() %>%
        as.data.frame() %>%
        rename(i = V1, j = V2)

    expect_true(nrow(anti_join(pairs, res, by = c("i", "j"))) == 0)
})

test_that("emr_cor works when dataframe = TRUE", {
    # The warning is due to the bit iterator
    expect_warning(res <- emr_cor("track0", c(0, 10, 500, 1000), cor.exprs = c("track0", "track1", "track2", "track3"), iterator = 1, stime = 20, etime = 5000, keepref = F, dataframe = TRUE, names = "savta"))

    expect_equal(colnames(res), c("savta", "i", "j", "n", "e", "var", "cov", "cor"))
})
