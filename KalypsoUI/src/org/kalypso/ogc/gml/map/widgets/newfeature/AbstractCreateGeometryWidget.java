package org.kalypso.ogc.gml.map.widgets.newfeature;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.builders.IGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.MultiPolygonGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PointGeometryBuilder;
import org.kalypso.ogc.gml.map.widgets.builders.PolygonGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Gernot Belger
 */
public abstract class AbstractCreateGeometryWidget extends AbstractWidget
{
  private final List<IGeometryBuilder> m_buildersToDraw = new ArrayList<IGeometryBuilder>();

  private Point m_currentPoint = null;

  private IGeometryBuilder m_builder = null;

  private int m_currentGeometry = -1;

  private IKalypsoFeatureTheme m_theme;

  private final QName m_qname;

  private IFeatureType m_featureType;

  private final QName[] m_geomProperties;

  /**
   * @param qname
   *            The qname of the feature to create
   * @param geomProperties
   *            The properties of the feature which shall be edited (in the given order).
   */
  public AbstractCreateGeometryWidget( final QName qname, final QName[] geomProperties )
  {
    super( "New Feature", "Creates a new Feature" );

    m_qname = qname;
    m_geomProperties = geomProperties;
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

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    /* Deselect all */
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();

    super.finish();
  }

  private void reinit( )
  {
    m_buildersToDraw.clear();
    m_theme = null;
    m_currentGeometry = -1;
    m_featureType = null;

    /* Get the active theme */
    final IKalypsoTheme activeTheme = getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) activeTheme;
      final IFeatureType featureType = theme == null ? null : theme.getFeatureType();
      final IGMLSchema schema = featureType == null ? null : featureType.getGMLSchema();
      m_featureType = schema == null ? null : schema.getFeatureType( m_qname );
      if( m_geomProperties.length > 0 )
      {
        m_theme = theme;
        m_currentGeometry = 0;

        initNextBuilder();
        return;
      }
    }
  }

  private void initNextBuilder( )
  {
    if( m_currentGeometry > m_geomProperties.length - 1 )
    {
      m_builder = null;
      m_currentGeometry = -1;
      return;
    }

    final QName geomProp = m_geomProperties[m_currentGeometry];
    final IValuePropertyType vpt = (IValuePropertyType) m_featureType.getProperty( geomProp );
    final QName valueQName = vpt.getValueQName();
    final String targetCrs = getMapPanel().getMapModell().getCoordinatesSystem();

    if( GeometryUtilities.QN_POLYGON_PROPERTY.equals( valueQName ))      
      m_builder = new PolygonGeometryBuilder( 0, targetCrs );
    else if(GeometryUtilities.QN_MULTI_POLYGON_PROPERTY.equals( valueQName ))
      m_builder = new MultiPolygonGeometryBuilder( 0, targetCrs );
    else if( GeometryUtilities.QN_LINE_STRING_PROPERTY.equals( valueQName ) )
      m_builder = new LineGeometryBuilder( 0, targetCrs );
    else if(GeometryUtilities.QN_MULTI_LINE_STRING_PROPERTY.equals( valueQName ))
      m_builder = new LineGeometryBuilder( 0, targetCrs ); //TODO 
    else if( GeometryUtilities.QN_POINT_PROPERTY.equals( valueQName ) )
      m_builder = new PointGeometryBuilder( targetCrs );
    else
    {
      m_builder = null;
      m_currentGeometry = -1;
    }
  }

  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;

    // TODO: check if this repaint is necessary for the widget
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();
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
        nextProperty();
    }
    catch( final Exception e )
    {
      KalypsoGisPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
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
        nextProperty();
    }
    catch( final Exception e )
    {
      KalypsoGisPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    switch( e.getKeyCode() )
    {
      case KeyEvent.VK_ESCAPE:
        e.consume();
        reinit();
        getMapPanel().repaint();
        break;

      default:
        break;
    }
  }

  private void nextProperty( ) throws Exception
  {
    m_buildersToDraw.add( m_builder );

    m_currentGeometry++;
    initNextBuilder();

    if( m_currentGeometry == -1 )
    {
      final FeatureList featureList = m_theme.getFeatureList();
      final Feature parentFeature = featureList.getParentFeature();
      final IRelationType parentRelation = featureList.getParentFeatureTypeProperty();
      final Feature newFeature = parentFeature.getWorkspace().createFeature( parentFeature, parentRelation, m_featureType );

      // set builded geometries to feature
      for( int i = 0; i < m_geomProperties.length; i++ )
      {
        final QName geomProp = m_geomProperties[i];
        final GM_Object geom = m_buildersToDraw.get( i ).finish();
        newFeature.setProperty( geomProp, geom );
      }

      // add new feature
      final IRelationType rt = m_theme.getFeatureList().getParentFeatureTypeProperty();
      final CommandableWorkspace workspace = m_theme.getWorkspace();
      final ICommand command = new AddFeatureCommand( workspace, parentFeature, rt, -1, newFeature, null );
      try
      {
        workspace.postCommand( command );

        /* Also select the newly created feature */
        final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
        selectAndShowFeatures( workspace, new Feature[] { newFeature }, selectionManager );
      }
      catch( final Exception e )
      {
        KalypsoGisPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }

      reinit();
    }
  }

  /* Public in order to be called as utility method. Maybe use elsewhere? */
  public static void selectAndShowFeatures( final CommandableWorkspace workspace, final Feature[] selectedFeatures, final IFeatureSelectionManager selectionManager )
  {
    final Feature featureToSelect = selectedFeatures[0];
    final EasyFeatureWrapper easyToSelect = new EasyFeatureWrapper( workspace, featureToSelect, featureToSelect.getParent(), featureToSelect.getParentRelation() );

    final Feature[] featuresToRemove = FeatureSelectionHelper.getFeatures( selectionManager );
    selectionManager.changeSelection( featuresToRemove, new EasyFeatureWrapper[] { easyToSelect } );

    /* Open the feature view */
    final Display display = PlatformUI.getWorkbench().getDisplay();
    display.asyncExec( new Runnable()
    {
      public void run( )
      {
        try
        {
          PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE );
        }
        catch( final Throwable pie )
        {
          // final IStatus status = StatusUtilities.statusFromThrowable( pie );
          // KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
          pie.printStackTrace();
        }
      }
    } );
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
