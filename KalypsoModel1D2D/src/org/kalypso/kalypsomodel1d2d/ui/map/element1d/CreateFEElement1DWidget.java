package org.kalypso.kalypsomodel1d2d.ui.map.element1d;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryHelper;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * Widget for creating 1d2d element
 * 
 * @author Patrice Congo
 */
public class CreateFEElement1DWidget extends AbstractWidget
{
  private final int m_grabRadius = 20;

  private LineGeometryBuilder m_lineBuilder = null;

  private IKalypsoFeatureTheme m_theme;

  private IFEDiscretisationModel1d2d m_model1d2d;

  private GM_Point m_currentPos = null;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private IFE1D2DNode m_node;

  public CreateFEElement1DWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  private final void reinit( )
  {
    final IMapPanel mapPanel = getMapPanel();

    mapPanel.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.2" ) ); //$NON-NLS-1$

    final IMapModell mapModell = mapPanel.getMapModell();
    m_theme = UtilMap.findEditableTheme( mapPanel, IElement1D.QNAME );
    m_model1d2d = UtilMap.findFEModelTheme( mapPanel );
    m_lineBuilder = new LineGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
  }

  @Override
  public void moved( final Point p )
  {
    m_currentPos = MapUtilities.transform( getMapPanel(), p );

    /* find node */
    m_node = null;

    final double grabDistance = MapUtilities.calculateWorldDistance( getMapPanel(), m_currentPos, m_grabRadius * 2 );
    if( m_model1d2d == null )
      return;

    m_node = m_model1d2d.findNode( m_currentPos, grabDistance );

    final IMapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaintMap();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    final IMapPanel mapPanel = getMapPanel();

    /* If we have a node, take this position, else take the current one */
    final GM_Point currentPos = m_node == null ? MapUtilities.transform( mapPanel, p ) : m_node.getPoint();

    mapPanel.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.3" ) ); //$NON-NLS-1$

    try
    {
      final GM_Curve curve = (GM_Curve) m_lineBuilder.addPoint( currentPos );
      if( curve != null )
        finishLine( curve );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      mapPanel.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.4" ) + status.getMessage() ); //$NON-NLS-1$
      reinit();
    }
  }

  /**
   * TODO: change to right-clicked: BUT!: at the moment the xontext menu is opened, so the framework must know wether
   * this widget is editing something at the moment or not
   * 
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    if( m_lineBuilder != null )
    {
      try
      {
        final GM_Curve curve = (GM_Curve) m_lineBuilder.finish();
        // finishLine( curve );
        final ICommand command = finishLine2( curve );
        if( command != null )
        {
          m_theme.getWorkspace().postCommand( command );
          reinit();
        }

      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
        final IMapPanel mapPanel = getMapPanel();
        mapPanel.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.5" ) + status.getMessage() ); //$NON-NLS-1$
        reinit();
      }
    }
  }

  public void finishLine( final GM_Curve curve ) throws GM_Exception
  {
    /* create 1d elements */
    final String crs = curve.getCoordinateSystem();

    final CommandableWorkspace workspace = m_theme.getWorkspace();
    final ChangeDiscretiationModelCommand modelChangeCmd = new ChangeDiscretiationModelCommand( workspace, m_model1d2d );

    final int numberOfCurveSegments = curve.getNumberOfCurveSegments();
    for( int i = 0; i < numberOfCurveSegments; i++ )
    {
      final GM_CurveSegment segment = curve.getCurveSegmentAt( i );

      final int numberOfPoints = segment.getNumberOfPoints();

      AddNodeCommand lastNodeCmd = null;

      for( int j = 0; j < numberOfPoints - 1; j++ )
      {
        final GM_Position startPosition = segment.getPositionAt( j );
        final GM_Position endPosition = segment.getPositionAt( j + 1 );

        final GM_Point startPoint = GeometryFactory.createGM_Point( startPosition, crs );
        final GM_Point endPoint = GeometryFactory.createGM_Point( endPosition, crs );

        // ElementGeometryHelper.createAddElement( command, workspace, parentFeature, parentNodeProperty,
        // parentEdgeProperty, parentElementProperty, nodeContainerPT, edgeContainerPT, m_model1d2d, nodes );

        final AddNodeCommand addNode0 = lastNodeCmd != null ? lastNodeCmd : new AddNodeCommand( m_model1d2d, startPoint, m_grabRadius );
        final AddNodeCommand addNode1 = new AddNodeCommand( m_model1d2d, endPoint, m_grabRadius );
        lastNodeCmd = addNode1;
        final Add1DElementFromNodeCmd eleCmd = new Add1DElementFromNodeCmd( m_model1d2d, new AddNodeCommand[] { addNode0, addNode1 } );
        modelChangeCmd.addCommand( addNode0 );
        modelChangeCmd.addCommand( addNode1 );
        modelChangeCmd.addCommand( eleCmd );
      }
    }

    m_theme.postCommand( modelChangeCmd, null );

    reinit();
  }

  private ICommand finishLine2( final GM_Curve curve ) throws GM_Exception
  {
    final CompositeCommand command = new CompositeCommand( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.6" ) ); //$NON-NLS-1$

    final CommandableWorkspace workspace = m_theme.getWorkspace();
    final FeatureList featureList = m_theme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();

    /* Initialize elements needed for edges and elements */
    final IFEDiscretisationModel1d2d discModel = new FE1D2DDiscretisationModel( parentFeature );

    /* create 1d elements */
    final String crs = curve.getCoordinateSystem();

    final int numberOfCurveSegments = curve.getNumberOfCurveSegments();

    for( int i = 0; i < numberOfCurveSegments; i++ )
    {
      final GM_CurveSegment segment = curve.getCurveSegmentAt( i );
      final int numberOfPoints = segment.getNumberOfPoints();

      for( int j = 0; j < numberOfPoints - 1; j++ )
      {
        final List<GM_Point> nodes = new ArrayList<GM_Point>();

        final GM_Position startPosition = segment.getPositionAt( j );
        final GM_Position endPosition = segment.getPositionAt( j + 1 );

        final GM_Point startPoint = GeometryFactory.createGM_Point( startPosition, crs );
        final GM_Point endPoint = GeometryFactory.createGM_Point( endPosition, crs );

        nodes.add( startPoint );
        nodes.add( endPoint );

        ElementGeometryHelper.createAdd1dElement( command, workspace, parentFeature, discModel, nodes );
      }
    }

    return command;

    // reinit();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    super.paint( g );

    if( m_currentPos != null )
    {
      final Point currentPoint = MapUtilities.retransform( getMapPanel(), m_currentPos );
      if( m_lineBuilder != null )
        m_lineBuilder.paint( g, getMapPanel().getProjection(), currentPoint );
    }

    if( m_node != null )
    {
      final int smallRect = 10;
      final Point nodePoint = MapUtilities.retransform( getMapPanel(), m_node.getPoint() );
      g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
    }

    final Rectangle bounds = mapPanel.getScreenBounds();

    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget.7" ) ); //$NON-NLS-1$
    m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    final int keyCode = e.getKeyCode();
    switch( keyCode )
    {
      case KeyEvent.VK_ESCAPE:
        reinit();
        getMapPanel().repaintMap();
        break;

      case KeyEvent.VK_BACK_SPACE:
        m_lineBuilder.removeLastPoint();
        getMapPanel().repaintMap();
        break;

      default:
        break;
    }
  }

}
