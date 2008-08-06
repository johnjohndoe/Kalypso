package org.kalypso.model.wspm.sobek.calculation.job;

public interface ISobekCalculationJobConstants
{
  public static String CALC_CASE_PATH = "CALC_CASE_PATH"; //$NON-NLS-1$

  public static String FLOW_NETWORK_PATH = "FLOW_NETWORK_PATH"; //$NON-NLS-1$

  public static String CALCULATION_RESULT_POINTS = "CALCULATION_RESULT_POINTS"; //$NON-NLS-1$

  public static String CALCULATION_RESULT_STRUCTURES = "CALCULATION_RESULT_STRUCTURES"; //$NON-NLS-1$

  public static String LOG_PI2SOBEK = "LOG_PI2SOBEK"; //$NON-NLS-1$

  public static String PATH_SOBEK_DIR = "Sobek-IDSS"; //$NON-NLS-1$

  public static String PATH_SOBEK_BATCH_DIR = PATH_SOBEK_DIR + "/batch"; //$NON-NLS-1$

  public static String PATH_SOBEK_DATA_DIR = PATH_SOBEK_DIR + "/data"; //$NON-NLS-1$

  public static String PATH_SOBEK_DATA_CALCULATION_RESULT_DIR = PATH_SOBEK_DATA_DIR + "/OpenMI2PIdyn"; //$NON-NLS-1$

  public static String CALCULATION_RESULT_POINTS_PATH = PATH_SOBEK_DATA_CALCULATION_RESULT_DIR + "/calcpnt.xml"; //$NON-NLS-1$

  public static String CALCULATION_RESULT_STRUCTURES_PATH = PATH_SOBEK_DATA_CALCULATION_RESULT_DIR + "/struc.xml"; //$NON-NLS-1$

  public static String LOG_PI2SOBEK_PATH = PATH_SOBEK_DATA_DIR + "/sobek_river/CMTWORK/conv_sbk.log"; //$NON-NLS-1$

  public static String LOG_OPENMI_CONTROL = "LOG_OPENMI_CONTROL"; //$NON-NLS-1$

  public static String LOG_OPENMI_CONTROL_PATH = PATH_SOBEK_DATA_DIR + "/iDSS2OpenMI/CompositionRun.log"; //$NON-NLS-1$

  public static String LOG_SOBEK = "LOG_SOBEK"; //$NON-NLS-1$

  public static String LOG_SOBEK_PATH = PATH_SOBEK_DATA_DIR + "/sobek_river/work/sobek.log"; //$NON-NLS-1$

  public static String LOG_SOBEK2PI_POINTS = "LOG_SOBEK2PI_POINTS"; //$NON-NLS-1$

  public static String LOG_SOBEK2PI_POINTS_PATH = PATH_SOBEK_DATA_CALCULATION_RESULT_DIR + "/diag_calcpnt_hisxmlconverter.xml"; //$NON-NLS-1$

  public static String LOG_SOBEK2PI_STRUCTURES = "LOG_SOBEK2PI_STRUCTURES"; //$NON-NLS-1$

  public static String LOG_SOBEK2PI_STRUCTURES_PATH = PATH_SOBEK_DATA_CALCULATION_RESULT_DIR + "/diag_struc_hisxmlconverter.xml"; //$NON-NLS-1$

}
