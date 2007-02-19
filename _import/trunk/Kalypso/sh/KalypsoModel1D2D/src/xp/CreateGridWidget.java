package xp;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Provides the mechanism to create automaticaly fem element
 * within a grid
 *  
 * @author Patrice Congo
 */
public class CreateGridWidget extends AbstractWidget implements IWidgetWithOptions
{
  private Point m_currentPoint = null;
  //private LineGeometryBuilder m_builder;//s[3] = new Linenull;
  GridPointCollector gridPointCollector= 
                                        new GridPointCollector();
  private boolean isActivated=false;
  
  private int m_radius=20;
  
  
  
  public CreateGridWidget( )
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
    if(gridPointCollector.getHasAllSides())
    {
      //gridPointCollector.selectPoint( lowerCorner, upperCorner );
      final int centerX=(int)p.getX();
      final int centerY=(int)p.getY();
      
      int halfWidth=m_radius/2;
      Point lower=new Point(centerX-halfWidth, centerY-halfWidth);
      Point upper=new Point(centerX+halfWidth, centerY+halfWidth);
      
      MapPanel mapPanel=getMapPanel();
      GM_Point gmLower=MapUtilities.transform( mapPanel, lower );
      gridPointCollector.selectPoint( 
          gmLower,
          MapUtilities.calculateWorldDistance( mapPanel, gmLower, m_radius*2 ));
      System.out.println(
          "selected On moved="+gridPointCollector.getSelectedPoint());
      
    }
    
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    System.out.println("Click="+p);
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
//      final GM_Curve curve = (GM_Curve) m_builder.finish();
      gridPointCollector.finishSide();
      System.out.println("DoubleClick:"+p);
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

  private Point draggedPoint; 
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {
    m_currentPoint = p;
    try
    {
      if(gridPointCollector.getHasAllSides())
      {
        GM_Point selectedPoint =
                gridPointCollector.getSelectedPoint();
        if(selectedPoint!=null)
        {
          System.out.println(
                        "HasAllSides:"+selectedPoint.getX() +
                        " "+ selectedPoint.getY());
        }
        if( isSamePoint( 
                  p, 
                  gridPointCollector.getSelectedPoint(),
                  m_radius*2, 
                  getMapPanel().getProjection() ) )
        {
          System.out.println("HasAllSides:dragged change posted");
            GM_Point nextPoint=
                MapUtilities.transform( getMapPanel(), p );
            gridPointCollector.changeSelectedPoint( nextPoint );
            //getActiveTheme().fireModellEvent( null );
            //getMapPanel().getMapModell().fireModellEvent( null );
            getMapPanel().getMapModell().getActiveTheme().fireModellEvent( null );
        }
        else
        {
          System.out.println("HasAllSides: no dragged change posted");
        }
      }
      else
      {
        draggedPoint = p;
        System.out.println( "Dragged point=" + p );
        GM_Point lastSaved = gridPointCollector.getLastPoint();
        GeoTransform transform=getMapPanel().getProjection();
        if( isSamePoint( p, lastSaved, m_radius, getMapPanel().getProjection() ) )
        {
            
        }
        else if( isSamePoint( m_currentPoint, p, m_radius ) )
        {
          m_currentPoint = p;
          return;
        }
        final GM_Point currentPos = MapUtilities.transform( getMapPanel(), p );
        gridPointCollector.replaceLastPoint( currentPos );
        m_currentPoint = p;
      }
      
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }    
    
  }
  
  public static final boolean isSamePoint(
                                  Point ref, 
                                  GM_Point toCompare, 
                                  int m_radius,
                                  GeoTransform transform
                                  )
  {
    Assert.throwIAEOnNull( transform, "transform must not be null" );
    if(ref==null || toCompare==null)
    {
      return false;
    }
    else
    {
      final int x = (int) transform.getDestX( toCompare.getX() );
      final int y = (int) transform.getDestY( toCompare.getY() );
      return (x > (ref.getX() - m_radius)) && 
                (x > (ref.getY() - m_radius)) && 
                (y < (ref.getX() + m_radius)) && 
                (y < (ref.getY() + m_radius)); 
    }
    
  }
  
  public static final boolean isSamePoint(
                                Point ref, Point toCompare, int m_radius)
  {
    if(ref==null)
    {
      return false;
    }
    else
    {
      return (toCompare.getX() > (ref.getX() - m_radius)) && 
                (toCompare.getY() > (ref.getY() - m_radius)) && 
                (toCompare.getX() < (ref.getX() + m_radius)) && 
                (toCompare.getY() < (ref.getY() + m_radius)); 
        
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
    MapPanel mapPanel=getMapPanel();
    
    if(e.isActionKey())
    {
      System.out.println("e:"+e);
    }
    if(typed==ESC)
    {
      if(e.isShiftDown())
      {
        reinit();
        mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );//paint();
      }
      else
      {
        gridPointCollector.clearCurrent();
        //mapPanel.getMapModell().addModellListener( listener )
        //TODO get the geometry redrawn
        mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );//paint();
      }
      
    }
    else if(typed=='\b')
    {
      
      if(e.isShiftDown())//e.getModifiers()==InputEvent.SHIFT_MASK)
      {
       gridPointCollector.gotoPreviousSide(); 
       mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );
      }
      else
      {
        gridPointCollector.removeLastPoint();
        //mapPanel.getMapModell().addModellListener( listener )
        //TODO get the geometry redrawn
        mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );//paint();
      }      
    }
    else if(typed=='\t')
    {
      System.out.println("Selected");
    }
    else if(typed=='t')
    {
      IMapModell mapModel=getMapPanel().getMapModell();
      IFEDiscretisationModel1d2d model1d2d = 
                 UtilMap.findFEModelTheme(  
                                mapModel, 
                                Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      IKalypsoFeatureTheme theme=
      UtilMap.findEditableThem(  
           mapModel, 
           Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
      
      CommandableWorkspace workspace = theme.getWorkspace();
      try
      {
        ICommand command = 
          gridPointCollector.getAddToModelCommand( 
              mapPanel,model1d2d,workspace );
        
        workspace.postCommand( command );
        reinit();//gridPointCollector.reset( mapModel.getCoordinatesSystem() );
        
        
        System.out.println("GridCommand posted");
      }
      catch( Throwable e1 )
      {
        e1.printStackTrace();
      }
    }
    else
    {
      
      System.out.println("Char="+typed);
    }
    
  }
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#rightClicked(java.awt.Point)
   */
  @Override
  public void rightClicked( Point p )
  {
    
  }
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#middleClicked(java.awt.Point)
   */
  @Override
  public void middleClicked( Point p )
  {
    gridPointCollector.selectNext();
    MapPanel mapPanel=getMapPanel();
    mapPanel.getMapModell().getActiveTheme().fireModellEvent( null );
  }
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    System.out.println("FINISH");
   super.finish();
   //isActivated=false;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  @Override
  public void setSelection( ISelection selection )
  {
    System.out.println("Sel="+selection);
  }

  
  private GridWidgetFace gridWidgetFace = new GridWidgetFace(this);
  
  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( Composite parent )
  {
     Control control = gridWidgetFace.createControl( parent );
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
  
   
}
