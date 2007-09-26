/*
 * Insert INFORM.DSS licence here.
 */
package org.kalypso.kalypso1d2d.pjt;

import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey;

/**
 * Utility class for handling images in this plugin.
 * 
 * @author belger
 */
public class Kalypso1d2dProjectImages
{
  public static enum DESCRIPTORS implements ImageKey
  {
    RESULT_META_SCENARIO("icons/cview16/resViewScenario.gif"),

    RESULT_META_CALC_UNIT("icons/cview16/resViewCalcUnit.gif"),

    RESULT_META_DOCUMENT_NODES("icons/cview16/resViewDocNodes.gif"),
    RESULT_META_DOCUMENT_WSP("icons/cview16/resViewDocWSP.gif"),
    RESULT_META_DOCUMENT_Depth("icons/cview16/resViewDocDepth.gif"),
    RESULT_META_DOCUMENT_Velo("icons/cview16/resViewDocVelo.gif"),
    RESULT_META_DOCUMENT_TIN("icons/cview16/resViewDocTIN.gif"),
    RESULT_META_DOCUMENT_HYDRO("icons/cview16/resViewDocHydro.gif"),
    RESULT_META_DOCUMENT_LENGTH_SECTION("icons/cview16/resViewDocLengthSec.gif"),
    RESULT_META_DOCUMENT_DIFFERENCES("icons/cview16/resViewDocDiff.gif"),

    RESULT_META_DOCUMENT_LOG("icons/cview16/resViewDocLog.gif"),
    RESULT_META_DOCUMENT_ZIP("icons/cview16/resViewDocZip.gif"),

    RESULT_META_STEP_STEADY("icons/cview16/resViewStepSteady.gif"),
    RESULT_META_STEP_UNSTEADY("icons/cview16/resViewStepUnsteady.gif"),
    RESULT_META_STEP_MAX("icons/cview16/resViewStepMax.gif"),
    RESULT_META_STEP_QSTEADY("icons/cview16/resViewStepQsteady.gif"),

    RESULT_META_ERROR("icons/cview16/resViewDocError2.gif"),

    RESULT_VIEWER_EDIT("icons/cview16/resViewEdit.gif"),
    RESULT_VIEWER_REMOVE("icons/cview16/resViewRemove.gif");

    private final String m_imagePath;

    private DESCRIPTORS( final String imagePath )
    {
      m_imagePath = imagePath;
    }

    /**
     * @see org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey#getImagePath()
     */
    public String getImagePath( )
    {
      return m_imagePath;
    }
  }
}