package org.kalypso.model.wspm.sobek.calculation.job;

public interface ISobekCalculationJobConstants
{
  public static String CALC_CASE_PATH = "CALC_CASE_PATH";

  public static String FLOW_NETWORK_PATH = "FLOW_NETWORK_PATH";

  public static String CALCULATION_RESULT_POINTS = "CALCULATION_RESULT_POINTS";

  public static String CALCULATION_RESULT_STRUCTURES = "CALCULATION_RESULT_STRUCTURES";

  public static String LOG_PI2SOBEK = "LOG_PI2SOBEK";

  public static String PATH_SOBEK_DIR = "Sobek-IDSS";

  public static String PATH_SOBEK_BATCH_DIR = PATH_SOBEK_DIR + "/batch";

  public static String PATH_SOBEK_DATA_DIR = PATH_SOBEK_DIR + "/data";

  public static String PATH_SOBEK_DATA_CALCULATION_RESULT_DIR = PATH_SOBEK_DATA_DIR + "/OpenMI2PIdyn";

  public static String CALCULATION_RESULT_POINTS_PATH = PATH_SOBEK_DATA_CALCULATION_RESULT_DIR + "/calcpnt.xml";

  public static String CALCULATION_RESULT_STRUCTURES_PATH = PATH_SOBEK_DATA_CALCULATION_RESULT_DIR + "/struc.xml";

  public static String LOG_PI2SOBEK_PATH = PATH_SOBEK_DATA_DIR + "/sobek_river/CMTWORK/conv_sbk.log";

  public static String LOG_OPENMI_CONTROL = "LOG_OPENMI_CONTROL";

  public static String LOG_OPENMI_CONTROL_PATH = PATH_SOBEK_DATA_DIR + "/iDSS2OpenMI/CompositionRun.log";

  public static String LOG_SOBEK = "LOG_SOBEK";

  public static String LOG_SOBEK_PATH = PATH_SOBEK_DATA_DIR + "/sobek_river/work/sobek.log";

  public static String LOG_SOBEK2PI_POINTS = "LOG_SOBEK2PI_POINTS";

  public static String LOG_SOBEK2PI_POINTS_PATH = PATH_SOBEK_DATA_CALCULATION_RESULT_DIR + "/diag_calcpnt_hisxmlconverter.xml";

  public static String LOG_SOBEK2PI_STRUCTURES = "LOG_SOBEK2PI_STRUCTURES";

  public static String LOG_SOBEK2PI_STRUCTURES_PATH = PATH_SOBEK_DATA_CALCULATION_RESULT_DIR + "/diag_struc_hisxmlconverter.xml";

}
