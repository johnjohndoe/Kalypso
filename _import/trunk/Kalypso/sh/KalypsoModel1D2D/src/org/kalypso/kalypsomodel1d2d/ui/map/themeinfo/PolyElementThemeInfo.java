package org.kalypso.kalypsomodel1d2d.ui.map.themeinfo;

import java.util.Formatter;

import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.ogc.gml.FeatureThemeInfo;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;

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
      final GM_Surface<GM_SurfacePatch> surface = polyElement.getGeometry();
      final PolyChecker polyChecker = new PolyChecker( surface );

      formatter.format( "Area: %.2f m²%n", polyChecker.getArea() );
      formatter.format( "Max. Edge-Length: %.2f m%n", polyChecker.getMaxLength() );
      formatter.format( "Max. Edge-Ratio: 1 : %.1f%n%n", polyChecker.getLengthRatio() );

      final IRoughnessClsCollection roughnessModel = dataProvider.getModel( IRoughnessClsCollection.class.getName(), IRoughnessClsCollection.class );

      final String roughnessStyle = polyElement.getRoughnessStyle();
      formatter.format( "Rauheitsklasse: %s%n", roughnessStyle );

      final String roughnessClsID = polyElement.getRoughnessClsID();
      final Double roughnessCorrectionKS = polyElement.getRoughnessCorrectionKS();
      final Double roughnessCorrectionAXAY = polyElement.getRoughnessCorrectionAxAy();
      final Double roughnessCorrectionDP = polyElement.getRoughnessCorrectionDP();

      final Feature roughnessFeature = roughnessModel.getFeature().getWorkspace().getFeature( roughnessClsID );
      if( roughnessFeature == null )
        formatter.format( "Unknwon roughness class-id: %s", roughnessClsID );

      final IRoughnessCls roughnessCls = (IRoughnessCls) roughnessFeature.getAdapter( IRoughnessCls.class );

      final double ks = roughnessCls.getKs();
      if( roughnessCorrectionKS == null )
        formatter.format( "%nRoughness (ks):   %.3f m%n", ks );
      else
        formatter.format( "%nRoughness (ks):   %.3f m (%.2f%%)%n", ks, roughnessCorrectionKS * 100 );

      final double axay = roughnessCls.getAxAy();
      final double dp = roughnessCls.getDp();
      if( axay > 0 || dp > 0 )
      {
        if( roughnessCorrectionAXAY == null && roughnessCorrectionDP == null )
          formatter.format( "Vegetation (axay, dp): %.3f m, %.3f m%n", axay, dp );
        else
          formatter.format( "Vegetation (axay, dp): %.3f m (%.2f%%), %.3f m (%.2f%%)%n", axay, roughnessCorrectionAXAY * 100, dp, roughnessCorrectionDP * 100 );
      }

      final double eddyXX = roughnessCls.getEddyXX();
      final double eddyXY = roughnessCls.getEddyXY();
      final double eddyYX = roughnessCls.getEddyYX();
      final double eddyYY = roughnessCls.getEddyYY();
      formatter.format( "Eddy: %.3f / %.3f / %.3f / %.3f%n", eddyXX, eddyXY, eddyYX, eddyYY );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      formatter.format( "%s", e.getLocalizedMessage() );
    }
  }

  /**
   * Helper that calculates stuff for PolyElements
   */
  private final class PolyChecker
  {
    private final double m_maxLength;

    private final double m_minLength;

    private final double m_lengthRatio;

    private final double m_area;

    public PolyChecker( final GM_Surface<GM_SurfacePatch> surface )
    {
      final int surfaceSize = surface.size();
      double maxLength = 0.0;
      double minLength = Double.MAX_VALUE;
      for( int i = 0; i < surfaceSize; i++ )
      {
        final GM_SurfacePatch patch = surface.get( i );
        final GM_Position[] ring = patch.getExteriorRing();
        final int ringSize = ring.length;
        for( int j = 0; j < ringSize - 1; j++ )
        {
          final GM_Position pos0 = ring[j];
          final GM_Position pos1 = ring[j + 1];

          final double length = pos0.getDistance( pos1 );
          maxLength = Math.max( maxLength, length );
          minLength = Math.min( minLength, length );
        }
      }

      m_maxLength = maxLength;
      m_minLength = minLength;
      m_lengthRatio = maxLength / minLength;
      m_area = surface.getArea();
    }

    public double getArea( )
    {
      return m_area;
    }

    public double getMaxLength( )
    {
      return m_maxLength;
    }

    public double getLengthRatio( )
    {
      return m_lengthRatio;
    }
  }
}
