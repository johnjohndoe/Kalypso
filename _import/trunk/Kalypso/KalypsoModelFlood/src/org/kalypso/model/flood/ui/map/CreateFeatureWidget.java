package org.kalypso.model.flood.ui.map;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.flood.KalypsoModelFloodPlugin;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PointGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Gernot Belger
 */
public class CreateFeatureWidget extends AbstractWidget
{
  private Point m_currentPoint = null;

  private IGeometryBuilder m_builder = null;

  private List<IGeometryBuilder> m_buildersToDraw = new ArrayList<IGeometryBuilder>();

  private Feature m_newFeature = null;

  private int m_currentGeometry = -1;

  private IKalypsoFeatureTheme m_theme;

  public CreateFeatureWidget( )
  {
    super( "New Feature", "Creates a new Feature" );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    reinit();
  }
  
  private void reinit()
  {
    m_buildersToDraw.clear();
    m_newFeature = null;
    m_theme = null;
    m_currentGeometry = -1;

    /* Get the active theme */
    final IKalypsoTheme activeTheme = getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) activeTheme;
      final IValuePropertyType[] geomProperties = theme.getFeatureType().getAllGeomteryProperties();
      if( geomProperties.length > 0 )
      {
        m_theme = theme;
        m_currentGeometry = 0;

        final FeatureList featureList = m_theme.getFeatureList();
        final Feature parentFeature = featureList.getParentFeature();
        final IRelationType parentRelation = featureList.getParentFeatureTypeProperty();
        final IFeatureType featureType = featureList.getParentFeatureTypeProperty().getTargetFeatureType();
        m_newFeature = parentFeature.getWorkspace().createFeature( parentFeature, parentRelation, featureType );

        initBuilder( geomProperties );
        return;
      }
    }
  }

  private void initBuilder( final IValuePropertyType[] geomProperties )
  {
    final IValuePropertyType vpt = geomProperties[m_currentGeometry];
    final QName valueQName = vpt.getValueQName();
    final CS_CoordinateSystem targetCrs = getMapPanel().getMapModell().getCoordinatesSystem();

    if( GeometryUtilities.QN_POLYGON_PROPERTY.equals( valueQName ) )
      m_builder = new PolygonGeometryBuilder( 0, targetCrs );
    else if( GeometryUtilities.QN_LINE_STRING_PROPERTY.equals( valueQName ) )
      m_builder = new LineGeometryBuilder( 0, targetCrs );
    else if( GeometryUtilities.QN_POINT_PROPERTY.equals( valueQName ) )
      m_builder = new PointGeometryBuilder( targetCrs );
    else
    {
      m_newFeature = null;
      m_builder = null;
      m_currentGeometry = -1;
      m_buildersToDraw.clear();
    }
  }

  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    if( m_builder == null )
      return;

    try
    {
      final GM_Point currentPos = MapUtilities.transform( getMapPanel(), m_currentPoint );
      final GM_Object object = m_builder.addPoint( currentPos );
      if( object != null )
        nextProperty( object );
    }
    catch( final Exception e )
    {
      KalypsoModelFloodPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( m_builder == null )
      return;

    try
    {
      final GM_Object object = m_builder.finish();
      if( object != null )
        nextProperty( object );
    }
    catch( final Exception e )
    {
      KalypsoModelFloodPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  private void nextProperty( final GM_Object object )
  {
    final IFeatureType featureType = m_newFeature.getFeatureType();
    final IValuePropertyType[] allGeomteryProperties = featureType.getAllGeomteryProperties();
    final IValuePropertyType currentVpt = allGeomteryProperties[m_currentGeometry];
    m_newFeature.setProperty( currentVpt, object );

    if( m_currentGeometry == allGeomteryProperties.length - 1 )
    {
      // add new feature
      final IRelationType rt = m_theme.getFeatureList().getParentFeatureTypeProperty();
      final ICommand command = new AddFeatureCommand( m_theme.getWorkspace(), m_newFeature.getParent(), rt, -1, m_newFeature, null );
      try
      {
        m_theme.getWorkspace().postCommand( command );
      }
      catch( final Exception e )
      {
        KalypsoModelFloodPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }

      reinit();
    }
    else
    {
      m_buildersToDraw.add( m_builder );

      m_currentGeometry++;
      initBuilder( allGeomteryProperties );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Point currentPoint = m_currentPoint;

    if( currentPoint != null )
    {
      for( final IGeometryBuilder builder : m_buildersToDraw )
        builder.paint( g, getMapPanel().getProjection(), null );

      if( m_builder != null )
        m_builder.paint( g, getMapPanel().getProjection(), currentPoint );

      g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
    }
  }

}
