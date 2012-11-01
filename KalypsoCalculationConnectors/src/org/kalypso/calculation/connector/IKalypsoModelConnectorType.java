package org.kalypso.calculation.connector;


public interface IKalypsoModelConnectorType
{
  public enum MODEL_CONNECTOR_TYPEID
  {
    CONNECTOR_LZNA_KZNA,
    CONNECTOR_NA_WSPM,
    CONNECTOR_WSPM_FLOOD,
    CONNECTOR_FLOOD_RISK;
    public String getValue( )
    {
      final MODEL_CONNECTOR_TYPEID kind = MODEL_CONNECTOR_TYPEID.valueOf( name() );

      switch( kind )
      {
        case CONNECTOR_LZNA_KZNA:
          return "KalypsoModelConnector_LZNA_KZNA"; //$NON-NLS-1$
        case CONNECTOR_NA_WSPM:
          return "KalypsoModelConnector_NA_WSPM"; //$NON-NLS-1$
        case CONNECTOR_WSPM_FLOOD:
          return "KalypsoModelConnector_WSPM_FM"; //$NON-NLS-1$
        case CONNECTOR_FLOOD_RISK:
          return "KalypsoModelConnector_FM_RM"; //$NON-NLS-1$
        default:
          throw new UnsupportedOperationException();
      }
    }
  }

  public enum MODELSPEC_CONNECTOR_LZNA_KZNA
  {
    LZNA_ERGEBNISSE_AKTUEL_ANFANGWERTE,
    KZNA_CALCULATION,
    KZNA_ANFANGWERTE_LZSIM;
  }

  public enum MODELSPEC_CONNECTOR_NA_WSPM
  {
    NA_Model,
    NA_ControlModel,
    NA_StatisticalReport,
    NA_RiverCode,
    WSPM_Model,
    WSPM_RunoffEventID;
  }

  public enum MODELSPEC_CONNECTOR_WSPM_FM
  {
    WSPM_Model,
    WSPM_RunoffEventID,
    FM_Model,
    WSPM_TinFile,
    OPT_DeleteExistingRunoffEvents,
    WSPM_TinReference;
  }

  public enum MODELSPEC_CONNECTOR_FM_RM
  {
    FM_Model,
    FM_EventsFolder,
    RM_Model,
    RM_InputRasterFolder;
  }

}
