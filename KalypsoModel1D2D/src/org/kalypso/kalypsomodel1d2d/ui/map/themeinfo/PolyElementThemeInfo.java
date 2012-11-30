package org.kalypso.kalypsomodel1d2d.ui.map.themeinfo;

import java.util.Formatter;

import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessCls;
import org.kalypso.kalypsosimulationmodel.core.roughness.IRoughnessClsCollection;
import org.kalypso.ogc.gml.FeatureThemeInfo;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_AbstractSurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

public class PolyElementThemeInfo extends FeatureThemeInfo
{
  @Override
  public void formatInfo( final Formatter formatter, final Feature feature )
  {
    final IPolyElement polyElement = feature == null ? null : (IPolyElement)feature.getAdapter( IPolyElement.class );
    if( polyElement == null )
    {
      formatter.format( "-" ); //$NON-NLS-1$
      return;
    }

    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    if( dataProvider == null )
      return;

    try
    {
      final GM_Polygon surface = polyElement.getGeometry();
      final PolyChecker polyChecker = new PolyChecker( surface );

      formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.0" ), polyChecker.getArea() ); //$NON-NLS-1$
      formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.1" ), polyChecker.getMaxLength() ); //$NON-NLS-1$
      formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.2" ), polyChecker.getLengthRatio() ); //$NON-NLS-1$

      final IRoughnessClsCollection roughnessModel = dataProvider.getModel( IRoughnessClsCollection.class.getName() );

      final String roughnessStyle = polyElement.getRoughnessStyle();
      formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.3" ), roughnessStyle ); //$NON-NLS-1$

      final String roughnessClsID = polyElement.getRoughnessClsID();
      final Double roughnessCorrectionKS = polyElement.getRoughnessCorrectionKS();
      final Double roughnessCorrectionAXAY = polyElement.getRoughnessCorrectionAxAy();
      final Double roughnessCorrectionDP = polyElement.getRoughnessCorrectionDP();

      final Feature roughnessFeature = roughnessModel.getWorkspace().getFeature( roughnessClsID );
      if( roughnessFeature == null )
      {
        formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.4" ), roughnessClsID ); //$NON-NLS-1$
        return;
      }
      final IRoughnessCls roughnessCls = (IRoughnessCls)roughnessFeature.getAdapter( IRoughnessCls.class );

      final double ks = roughnessCls.getKs();
      if( roughnessCorrectionKS == null )
        formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.5" ), ks ); //$NON-NLS-1$
      else
        formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.6" ), ks, roughnessCorrectionKS * 100 ); //$NON-NLS-1$

      final double axay = roughnessCls.getAxAy();
      final double dp = roughnessCls.getDp();
      if( axay > 0 || dp > 0 )
      {
        if( roughnessCorrectionAXAY == null && roughnessCorrectionDP == null )
          formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.7" ), axay, dp ); //$NON-NLS-1$
        else
          formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.8" ), axay, roughnessCorrectionAXAY * 100, dp, roughnessCorrectionDP * 100 ); //$NON-NLS-1$
      }

      final double eddyXX = roughnessCls.getEddyXX();
      final double eddyXY = roughnessCls.getEddyXY();
      final double eddyYX = roughnessCls.getEddyYX();
      final double eddyYY = roughnessCls.getEddyYY();
      formatter.format( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.themeinfo.PolyElementThemeInfo.9" ), eddyXX, eddyXY, eddyYX, eddyYY ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      formatter.format( "%s", e.getLocalizedMessage() ); //$NON-NLS-1$
    }
  }

  /**
   * Helper that calculates stuff for PolyElements
   */
  private final class PolyChecker
  {
    private final double m_maxLength;

    // private final double m_minLength;

    private final double m_lengthRatio;

    private final double m_area;

    public PolyChecker( final GM_Polygon surface )
    {
      final int surfaceSize = surface.size();
      double maxLength = 0.0;
      double minLength = Double.MAX_VALUE;
      for( int i = 0; i < surfaceSize; i++ )
      {
        final GM_AbstractSurfacePatch patch = surface.get( i );
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
      // m_minLength = minLength;
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
