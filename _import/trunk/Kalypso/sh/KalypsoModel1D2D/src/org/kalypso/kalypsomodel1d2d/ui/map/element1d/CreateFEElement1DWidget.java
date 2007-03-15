package org.kalypso.kalypsomodel1d2d.ui.map.element1d;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.Add1DElementFromNodeCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.AddNodeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.grid.LinePointCollector;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Widget for creating 1d2d element
 * 
 * @author Patrice Congo
 */
public class CreateFEElement1DWidget extends AbstractWidget
{
  private Point m_currentPoint = null;

  private LinePointCollector linePointCollector= null;

  private IKalypsoFeatureTheme theme;

  private final int m_radius = 20;

  private IFEDiscretisationModel1d2d model1d2d;

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
    IMapModell mapModell = getMapPanel().getMapModell();
    theme =
      UtilMap.findEditableThem( 
                mapModell, 
                Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D );
    model1d2d=
      UtilMap.findFEModelTheme( 
            mapModell,                 
            Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D );
    linePointCollector= new LinePointCollector(2,mapModell.getCoordinatesSystem());
    
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
    final GM_Point currentPos = 
        MapUtilities.transform( getMapPanel(), m_currentPoint );
    
    try
    {
      if(linePointCollector.addPoint( currentPos )!=null)
      {
        //create 1d element;
        AddNodeCommand addNode0 = 
          new AddNodeCommand(
              model1d2d,
              linePointCollector.getPointAt( 0 ),
              m_radius);
        AddNodeCommand addNode1 = 
          new AddNodeCommand(
              model1d2d,
              linePointCollector.getPointAt( 1 ),
              m_radius);
        Add1DElementFromNodeCmd eleCmd=
                new Add1DElementFromNodeCmd(
                    model1d2d,new AddNodeCommand[]{addNode0,addNode1});
        CommandableWorkspace workspace = theme.getWorkspace();
        ChangeDiscretiationModelCommand modelChangeCmd=
          new ChangeDiscretiationModelCommand(workspace,model1d2d);
        modelChangeCmd.addCommand( addNode0 );
        modelChangeCmd.addCommand( addNode1 );
        modelChangeCmd.addCommand( eleCmd );
        theme.postCommand( modelChangeCmd, null );
        reinit();
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
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
      if( linePointCollector != null )
        linePointCollector.paint( g, getMapPanel().getProjection(), currentPoint, 10 );
      
      g.drawRect( 
          (int) currentPoint.getX() - 10, 
          (int) currentPoint.getY() - 10, 
          20, 
          20 );
    }
  }
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( KeyEvent e )
  {
    if(KeyEvent.VK_ESCAPE==e.getKeyCode())
    {
      this.reinit();
    }
  }

}
