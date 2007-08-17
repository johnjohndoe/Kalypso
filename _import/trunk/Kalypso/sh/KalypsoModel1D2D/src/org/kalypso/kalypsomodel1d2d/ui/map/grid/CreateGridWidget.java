package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provides the mechanism to create automaticaly fem element within a grid
 * 
 * @author Patrice Congo
 */
public class CreateGridWidget extends AbstractWidget implements IWidgetWithOptions
{
  private Point m_currentPoint = null;

  // private LineGeometryBuilder m_builder;//s[3] = new Linenull;
  GridPointCollector gridPointCollector = new GridPointCollector();

  private boolean isActivated = false;

  private final int m_radius = 20;

  private final IWidget m_delegateWidget = null;

  public CreateGridWidget( )
  {
    super( Messages.getString("CreateGridWidget.0"), Messages.getString("CreateGridWidget.1") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    // find the right themes to edit i.e. the discretisation model
    if( isActivated == false )
    {
      reinit();
      isActivated = true;
    }
  }

  private final void reinit( )
  {
    // m_builder = null;

    final CS_CoordinateSystem targetCrs = getMapPanel().getMapModell().getCoordinatesSystem();
    // m_builder = new LineGeometryBuilder( 0, targetCrs );
    gridPointCollector.reset( targetCrs );// SideToBuild( targetCrs );

  }

  @Override
  public void moved( final Point p )
  {
    m_currentPoint = p;
    if( gridPointCollector.getHasAllSides() )
    {
      // gridPointCollector.selectPoint( lowerCorner, upperCorner );
      final int centerX = (int) p.getX();
      final int centerY = (int) p.getY();

      final int halfWidth = m_radius / 2;
      final Point lower = new Point( centerX - halfWidth, centerY - halfWidth );
      final Point upper = new Point( centerX + halfWidth, centerY + halfWidth );

      final MapPanel mapPanel = getMapPanel();
      final GM_Point gmLower = MapUtilities.transform( mapPanel, lower );
      final GM_Point selectedPoint = gridPointCollector.getSelectedPoint();
      gridPointCollector.selectPoint( gmLower, MapUtilities.calculateWorldDistance( mapPanel, gmLower, m_radius * 2 ) );
      final GM_Point newSelectedPoint = gridPointCollector.getSelectedPoint();
      if( !ObjectUtils.equals( selectedPoint, newSelectedPoint ) )
      {
        System.out.println( "selected On moved=" + gridPointCollector.getSelectedPoint() ); //$NON-NLS-1$
        // TODO: check if this repaint is necessary for the widget
        final MapPanel panel = getMapPanel();
        if( panel != null )
          panel.repaint();
      }
    }
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
    System.out.println( "Click=" + p ); //$NON-NLS-1$
    try
    {
      // if( m_builder != null )
      // {
      // final GM_Point currentPos =
      // MapUtilities.transform( getMapPanel(), m_currentPoint );
      //
      // m_builder.addPoint( currentPos );
      // }

      final GM_Point currentPos = MapUtilities.transform( getMapPanel(), m_currentPoint );

      gridPointCollector.addPoint( currentPos );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#doubleClickedLeft(java.awt.Point)
   */
  @Override
  public void doubleClickedLeft( final Point p )
  {
    try
    {
      // final GM_Curve curve = (GM_Curve) m_builder.finish();
      gridPointCollector.finishSide();
      System.out.println( "DoubleClick:" + p ); //$NON-NLS-1$
      // validate geometry: doppelte punkte
    }
    catch( final Exception e )
    {

    }
    finally
    {
      // reinit();
    }
  }

  private Point draggedPoint;

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( final Point p )
  {
    m_currentPoint = p;
    try
    {
      if( gridPointCollector.getHasAllSides() )
      {
        final GM_Point selectedPoint = gridPointCollector.getSelectedPoint();
        if( selectedPoint != null )
        {
          // System.out.println( "HasAllSides:" + selectedPoint.getX() + " " + selectedPoint.getY() );
        }
        if( isSamePoint( p, gridPointCollector.getSelectedPoint(), m_radius * 2, getMapPanel().getProjection() ) || true )// TODO
        // remove
        // true
        {
          System.out.println( "HasAllSides:dragged change posted" ); //$NON-NLS-1$
          final GM_Point nextPoint = MapUtilities.transform( getMapPanel(), p );
          gridPointCollector.changeSelectedPoint( nextPoint );
          getMapPanel().repaint();
        }
        else
        {
          // System.out.println( "HasAllSides: no dragged change posted" );
        }
      }
      else
      {
        draggedPoint = p;
        // System.out.println( "Dragged point=" + p );
        final GM_Point lastSaved = gridPointCollector.getLastPoint();
        final GeoTransform transform = getMapPanel().getProjection();
        if( isSamePoint( p, lastSaved, m_radius, getMapPanel().getProjection() ) )
        {

        }
        else if( isSamePoint( m_currentPoint, p, m_radius ) )
        {
          m_currentPoint = p;
          // TODO: check if this repaint is really necessary
          final MapPanel panel = getMapPanel();
          if( panel != null )
            panel.repaint();

          return;
        }
        final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );
        gridPointCollector.replaceLastPoint( currentPos );
        m_currentPoint = p;
      }

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    // TODO: check if this repaint is really necessary
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();

  }

  public static final boolean isSamePoint( final Point ref, final GM_Point toCompare, final int m_radius, final GeoTransform transform )
  {
    Assert.throwIAEOnNull( transform, Messages.getString("CreateGridWidget.6") ); //$NON-NLS-1$
    if( ref == null || toCompare == null )
    {
      return false;
    }
    else
    {
      final int x = (int) transform.getDestX( toCompare.getX() );
      final int y = (int) transform.getDestY( toCompare.getY() );
      return (x > (ref.getX() - m_radius)) && (x > (ref.getY() - m_radius)) && (y < (ref.getX() + m_radius)) && (y < (ref.getY() + m_radius));
    }

  }

  public static final boolean isSamePoint( final Point ref, final Point toCompare, final int m_radius )
  {
    if( ref == null )
    {
      return false;
    }
    else
    {
      return (toCompare.getX() > (ref.getX() - m_radius)) && (toCompare.getY() > (ref.getY() - m_radius)) && (toCompare.getX() < (ref.getX() + m_radius))
          && (toCompare.getY() < (ref.getY() + m_radius));

    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Point currentPoint = m_currentPoint;

    // if( currentPoint != null )
    // {
    // if( m_builder != null )
    // m_builder.paint( g, getMapPanel().getProjection(), currentPoint );
    // g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
    // }

    if( currentPoint != null )
    {
      gridPointCollector.paint( g, getMapPanel().getProjection(), currentPoint );
      // TODO check what it is doing
      LinePointCollectorConfig currentLPCConfig = gridPointCollector.getCurrentLPCConfig();
      if( currentLPCConfig == null )
      {
        currentLPCConfig = gridPointCollector.getSideconfigsAsArray()[0];
      }

      final int pointRectSize = currentLPCConfig.getPointRectSize();
      final int pointRectSizeHalf = pointRectSize / 2;
// g.drawRect( (int) currentPoint.getX() - pointRectSizeHalf, (int) currentPoint.getY() - pointRectSizeHalf,
// pointRectSize, pointRectSize );

    }

  }

  public static final char ESC = 0X01B;

  /**
   * @param e
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    final int code = e.getKeyCode();

    final MapPanel mapPanel = getMapPanel();

    // TODO:
    // zoom in "-"

    if( e.isActionKey() )
    {
      System.out.println( "e:" + e ); //$NON-NLS-1$
    }
    /* zoom in */
    if( code == KeyEvent.VK_PLUS )
    {
      // if( e.isShiftDown() )
      // {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() - (currentMax.getX() - currentMin.getX()) / 10;
      final double newMaxY = currentMax.getY() - (currentMax.getY() - currentMin.getY()) / 10;
      final double newMinX = currentMin.getX() + (currentMax.getX() - currentMin.getX()) / 10;
      final double newMinY = currentMin.getY() + (currentMax.getY() - currentMin.getY()) / 10;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );

      // }
      // else
      // {
      // mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );
      // }

    }
    /* zoom out */
    else if( code == KeyEvent.VK_MINUS )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() + (currentMax.getX() - currentMin.getX()) / 10;
      final double newMaxY = currentMax.getY() + (currentMax.getY() - currentMin.getY()) / 10;
      final double newMinX = currentMin.getX() - (currentMax.getX() - currentMin.getX()) / 10;
      final double newMinY = currentMin.getY() - (currentMax.getY() - currentMin.getY()) / 10;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }

    // pan "arrows
    else if( code == KeyEvent.VK_RIGHT )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() + (currentMax.getX() - currentMin.getX()) / 20;
      final double newMinX = currentMin.getX() + (currentMax.getX() - currentMin.getX()) / 20;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, currentMin.getY() );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, currentMax.getY() );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_LEFT )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() - (currentMax.getX() - currentMin.getX()) / 20;
      final double newMinX = currentMin.getX() - (currentMax.getX() - currentMin.getX()) / 20;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, currentMin.getY() );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, currentMax.getY() );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_UP )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxY = currentMax.getY() + (currentMax.getY() - currentMin.getY()) / 20;
      final double newMinY = currentMin.getY() + (currentMax.getY() - currentMin.getY()) / 20;

      final GM_Position newMin = GeometryFactory.createGM_Position( currentMin.getX(), newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( currentMax.getX(), newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_DOWN )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxY = currentMax.getY() - (currentMax.getY() - currentMin.getY()) / 20;
      final double newMinY = currentMin.getY() - (currentMax.getY() - currentMin.getY()) / 20;

      final GM_Position newMin = GeometryFactory.createGM_Position( currentMin.getX(), newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( currentMax.getX(), newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    if( m_delegateWidget != null )
      m_delegateWidget.keyPressed( e );

    mapPanel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( final KeyEvent e )
  {
    final char typed = e.getKeyChar();
    final MapPanel mapPanel = getMapPanel();

    if( e.isActionKey() )
    {
      System.out.println( "e:" + e ); //$NON-NLS-1$
    }
    if( typed == ESC )
    {
      if( e.isShiftDown() )
      {
        reinit();
        mapPanel.repaint();
      }
      else
      {
        gridPointCollector.clearCurrent();
        mapPanel.repaint();
      }

    }
    else if( typed == '\b' )
    {

      if( e.isShiftDown() )// e.getModifiers()==InputEvent.SHIFT_MASK)
      {
        gridPointCollector.gotoPreviousSide();
        mapPanel.repaint();
      }
      else
      {
        gridPointCollector.removeLastPoint();
        mapPanel.repaint();
      }
    }
    else if( typed == '\t' )
    {
      System.out.println( "Selected" ); //$NON-NLS-1$
    }
    else if( typed == 't' )
    {
      convertToModell();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#rightClicked(java.awt.Point)
   */
  @Override
  public void rightClicked( final Point p )
  {

  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#middleClicked(java.awt.Point)
   */
  @Override
  public void middleClicked( final Point p )
  {
    gridPointCollector.selectNext();
    final MapPanel mapPanel = getMapPanel();
    mapPanel.repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    System.out.println( "FINISH" ); //$NON-NLS-1$
    super.finish();
    // isActivated=false;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( final ISelection selection )
  {
    System.out.println( "Sel=" + selection ); //$NON-NLS-1$
  }

  private final GridWidgetFace gridWidgetFace = new GridWidgetFace( this );

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    final Control control = gridWidgetFace.createControl( parent, toolkit, this );
    gridWidgetFace.setInput( gridPointCollector );
    return control;
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    gridWidgetFace.disposeControl();
  }

  public void convertToModell( )
  {
    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModel = getMapPanel().getMapModell();
    final IFEDiscretisationModel1d2d model1d2d = UtilMap.findFEModelTheme( mapModel );
    final IKalypsoFeatureTheme theme = UtilMap.findEditableTheme( mapModel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );

    final CommandableWorkspace workspace = theme.getWorkspace();
    try
    {
      final ICommand command = gridPointCollector.getAddToModelCommand( mapPanel, model1d2d, workspace );

      workspace.postCommand( command );
      reinit();// gridPointCollector.reset( mapModel.getCoordinatesSystem() );
    }
    catch( final Throwable e1 )
    {
      e1.printStackTrace();
    }
  }
}
