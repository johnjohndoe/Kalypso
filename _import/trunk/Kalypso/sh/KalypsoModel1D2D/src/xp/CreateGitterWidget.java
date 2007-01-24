package xp;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;


import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provides the mechanism to create automaticaly fem element
 * within a grid
 *  
 * @author Patrice Congo
 */
public class CreateGitterWidget extends AbstractWidget //implements IWidgetWithOptions
{
  private Point m_currentPoint = null;
  //private LineGeometryBuilder m_builder;//s[3] = new Linenull;
  private GridPointCollector gridPointCollector= 
                                        new GridPointCollector();
  private boolean isActivated=false;
  public CreateGitterWidget( )
  {
    super( 
        "New Grid complex element", 
        "Creates automaticaly element from a grid" );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( 
            final ICommandTarget commandPoster, 
            final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    // find the right themes to edit i.e. the discretisation model
    if(isActivated==false)
    {
      reinit();
      isActivated=true;
    }
  }

  private final void reinit( )
  {
//    m_builder = null;

    final CS_CoordinateSystem targetCrs = 
      getMapPanel().getMapModell().getCoordinatesSystem();
//    m_builder = new LineGeometryBuilder( 0, targetCrs );
    gridPointCollector.reset(targetCrs);//SideToBuild( targetCrs );
   
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
    try
    {
//      if( m_builder != null )
//      {
//        final GM_Point currentPos = 
//          MapUtilities.transform( getMapPanel(), m_currentPoint );
//
//        m_builder.addPoint( currentPos );
//      }
      final GM_Point currentPos = 
        MapUtilities.transform( getMapPanel(), m_currentPoint );

      gridPointCollector.addPoint( currentPos );
    }
    catch( final Exception e )
    {
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
//      final GM_Curve curve = (GM_Curve) m_builder.finish();
      gridPointCollector.finishSide();

      // validate geometry: doppelte punkte
    }
    catch( final Exception e )
    {
    
    }
    finally
    {
      //reinit();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    final Point currentPoint = m_currentPoint;

//    if( currentPoint != null )
//    {
//      if( m_builder != null )
//        m_builder.paint( g, getMapPanel().getProjection(), currentPoint );
//      g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
//    }
    
    if( currentPoint != null )
    {
      gridPointCollector.paint( g, getMapPanel().getProjection(), currentPoint );
      //TODO check what it is doing
      g.drawRect( 
          (int) currentPoint.getX() - 10, 
          (int) currentPoint.getY() - 10, 
          20, 
          20 );
    }
    
  }
  public static final char ESC=0X01B; 
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( KeyEvent e )
  {
    char typed=e.getKeyChar();
    if(typed==ESC)
    {
      gridPointCollector.clearCurrent();
      MapPanel mapPanel=getMapPanel();
      //mapPanel.getMapModell().addModellListener( listener )
      //TODO get the geometry redrawn
      mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );//paint();
      
    }
    else if(typed=='\b')
    {
      if(e.getModifiers()==InputEvent.SHIFT_MASK)
      {
       gridPointCollector.gotoPreviousSide(); 
      }
      else
      {
        gridPointCollector.removeLastPoint();
        MapPanel mapPanel=getMapPanel();
        //mapPanel.getMapModell().addModellListener( listener )
        //TODO get the geometry redrawn
        mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );//paint();
      }
      
    }
    else
    {
      System.out.println("Char="+typed);
    }
    //super.keyTyped(e);
  }
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    System.out.println("FINISH");
   super.finish();
   isActivated=false;
  }

}
