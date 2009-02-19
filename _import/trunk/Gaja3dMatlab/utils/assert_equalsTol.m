function assert_equalsTol( input1, input2 )
%% ASSERT_EQUALSTOL runs an assert_equals for two double arrays with a
%% tolerance of 10^-5. Treats NaN as equal.
    factor = 10^5;
    correctNaN1 = isnan(input1);
    correctNaN2 = isnan(input2);
    input1 = round(input1 * factor) / factor;
    input2 = round(input2 * factor) / factor;
    input1(correctNaN1) = 0;
    input2(correctNaN2) = 0;
    assert_equals(input1, input2)
end
