package org.kalypso.kalypsomodel1d2d.ui.map.themeinfo;

import java.util.Formatter;

import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.ogc.gml.FeatureThemeInfo;
import org.kalypsodeegree.model.feature.Feature;

public class PolyElementThemeInfo extends FeatureThemeInfo
{
  /**
   * @see org.kalypso.ogc.gml.FeatureThemeInfo#formatInfo(java.util.Formatter, org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  protected void formatInfo( final Formatter formatter, final Feature feature )
  {
    final IPolyElement polyElement = feature == null ? null : (IPolyElement) feature.getAdapter( IPolyElement.class );
    if( polyElement == null )
    {
      formatter.format( "-" );
      return;
    }

    final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
    if( dataProvider == null )
      return;

    try
    {
      final IRoughnessClsCollection roughnessModel = dataProvider.getModel( IRoughnessClsCollection.class.getName(), IRoughnessClsCollection.class );

      final String roughnessStyle = polyElement.getRoughnessStyle();
      formatter.format( "%s%n", roughnessStyle );

      final String roughnessClsID = polyElement.getRoughnessClsID();
      final Double roughnessCorrectionKS = polyElement.getRoughnessCorrectionKS();
      final Double roughnessCorrectionAXAY = polyElement.getRoughnessCorrectionAxAy();
      final Double roughnessCorrectionDP = polyElement.getRoughnessCorrectionDP();

      final Feature roughnessFeature = roughnessModel.getFeature().getWorkspace().getFeature( roughnessClsID );
      if( roughnessFeature == null )
        formatter.format( "Unknwon roughness class-id: %s", roughnessClsID );

      final IRoughnessCls roughnessCls = (IRoughnessCls) roughnessFeature.getAdapter( IRoughnessCls.class );

      final double ks = roughnessCls.getKs();
      final double ksCorr = roughnessCorrectionKS == null ? 1.0 : roughnessCorrectionKS;
      formatter.format( "%nRoughness (ks):   %.3f (%.3f)%n", ks, ksCorr );

      final double axay = roughnessCls.getAxAy();
      final double axayCorr = roughnessCorrectionAXAY == null ? 1.0 : roughnessCorrectionAXAY;
      final double dp = roughnessCls.getAxAy();
      final double dpCorr = roughnessCorrectionDP == null ? 1.0 : roughnessCorrectionDP;
      if( axay > 0 || dp > 0 )
        formatter.format( "Vegetation (axay, dp): %.3f, %.3f (%.3f, %.3f)%n", axay, dp, axayCorr, dpCorr );

      final double eddyXX = roughnessCls.getEddyXX();
      final double eddyXY = roughnessCls.getEddyXY();
      final double eddyYX = roughnessCls.getEddyYX();
      final double eddyYY = roughnessCls.getEddyYY();
      formatter.format( "eddy: %.3f / %.3f / %.3f / %.3f%n", eddyXX, eddyXY, eddyYX, eddyYY );

      final double marsh = roughnessCls.getMarsh();
      formatter.format( "marsh: %.3f%n", marsh );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      formatter.format( "%s", e.getLocalizedMessage() );
    }
  }

}
