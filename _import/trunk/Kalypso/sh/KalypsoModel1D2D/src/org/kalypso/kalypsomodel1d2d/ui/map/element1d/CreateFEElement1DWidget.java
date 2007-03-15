package org.kalypso.kalypsomodel1d2d.ui.map.element1d;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Widget for creating 1d2d element
 * 
 * @author Patrice Congo
 */
public class CreateFEElement1DWidget extends AbstractWidget
{
  private final int m_radius = 20;

  private Point m_currentPoint = null;

  private LineGeometryBuilder m_lineBuilder = null;

  private IKalypsoFeatureTheme m_theme;

  private IFEDiscretisationModel1d2d m_model1d2d;

  public CreateFEElement1DWidget( )
  {
    super( "Neues 1D FE-Element", "Ein neues 1D FE-Element kreieren" );
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

  private final void reinit( )
  {
    final MapPanel mapPanel = getMapPanel();

    mapPanel.setMessage( "Klicken Sie in die Karte um 1D-Elemente hinzuzufügen." );

    final IMapModell mapModell = mapPanel.getMapModell();
    m_theme = UtilMap.findEditableThem( mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D );
    m_model1d2d = UtilMap.findFEModelTheme( mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D );
    m_lineBuilder = new LineGeometryBuilder( 0, mapModell.getCoordinatesSystem() );
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
    final MapPanel mapPanel = getMapPanel();

    final GM_Point currentPos = MapUtilities.transform( mapPanel, p );

    mapPanel.setMessage( "Doppelklick: Beenden -  <ESC> abbrechen - <BACKSPACE> löschen" );

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
      mapPanel.setMessage( "Fehler: " + status.getMessage() );
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
        finishLine( curve );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
        final MapPanel mapPanel = getMapPanel();
        mapPanel.setMessage( "Fehler: " + status.getMessage() );
        reinit();
      }
    }
  }

  public void finishLine( final GM_Curve curve ) throws GM_Exception
  {
    /* create 1d elements */
    final CS_CoordinateSystem crs = curve.getCoordinateSystem();

    final int numberOfCurveSegments = curve.getNumberOfCurveSegments();
    for( int i = 0; i < numberOfCurveSegments; i++ )
    {
      final GM_CurveSegment segment = curve.getCurveSegmentAt( i );

      final int numberOfPoints = segment.getNumberOfPoints();
      for( int j = 0; j < numberOfPoints - 1; j++ )
      {
        final GM_Position startPosition = segment.getPositionAt( j );
        final GM_Position endPosition = segment.getPositionAt( j + 1 );

        final GM_Point startPoint = GeometryFactory.createGM_Point( startPosition, crs );
        final GM_Point endPoint = GeometryFactory.createGM_Point( endPosition, crs );

        final AddNodeCommand addNode0 = new AddNodeCommand( m_model1d2d, startPoint, m_radius );
        final AddNodeCommand addNode1 = new AddNodeCommand( m_model1d2d, endPoint, m_radius );

        final Add1DElementFromNodeCmd eleCmd = new Add1DElementFromNodeCmd( m_model1d2d, new AddNodeCommand[] { addNode0, addNode1 } );
        CommandableWorkspace workspace = m_theme.getWorkspace();
        ChangeDiscretiationModelCommand modelChangeCmd = new ChangeDiscretiationModelCommand( workspace, m_model1d2d );
        modelChangeCmd.addCommand( addNode0 );
        modelChangeCmd.addCommand( addNode1 );
        modelChangeCmd.addCommand( eleCmd );
        m_theme.postCommand( modelChangeCmd, null );
      }

    }

    reinit();
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
      if( m_lineBuilder != null )
        m_lineBuilder.paint( g, getMapPanel().getProjection(), currentPoint );

      g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
    }
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
        getMapPanel().repaint();
        break;

      case KeyEvent.VK_BACK_SPACE:
        m_lineBuilder.removeLast();
        getMapPanel().repaint();
        break;

      default:
        break;
    }
  }

}
