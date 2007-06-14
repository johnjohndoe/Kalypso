package org.kalypso.kalypsomodel1d2d.ui.map.del;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.swt.widgets.Display;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.MapKeyNavigator;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.ChangeDiscretiationModelCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Provide widget for deleting finit elements 
 * 
 * @author Patrice Congo
 */
public abstract class DeleteFEElementsWidget extends AbstractWidget implements WidgetStrategyContext
{
  private IWidget m_widgetStrategy;

  public DeleteFEElementsWidget( final IWidget widgetStrategy )
  {
    super( "Delete Finite element", "Delete finite element" );
    
    m_widgetStrategy = widgetStrategy;
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
    if(m_widgetStrategy!=null)
    {
      m_widgetStrategy.activate( commandPoster, mapPanel );
    }
  }

  private final void reinit( )
  {

  }

  @Override
  public void moved( final Point p )
  {
    try
    {
      if(m_widgetStrategy!=null)
      {
        m_widgetStrategy.moved( p );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
//  TODO: check if this repaint is necessary for the widget
    MapPanel panel = getMapPanel();
    if ( panel != null)
      panel.repaint();
    
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    try
    {
      if(m_widgetStrategy!=null)
      {
        m_widgetStrategy.leftClicked( p );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftPressed(java.awt.Point)
   */
  @Override
  public void leftPressed( Point p )
  {
    try
    {
      if(m_widgetStrategy!=null)
      {
        m_widgetStrategy.leftPressed( p );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }
  
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftReleased(java.awt.Point)
   */
  @Override
  public void leftReleased( Point p )
  {
    try
    {
      if(m_widgetStrategy!=null)
      {
        m_widgetStrategy.leftReleased( p );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
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
    try
    {
      if(m_widgetStrategy!=null)
      {
        m_widgetStrategy.doubleClickedLeft( p );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
//    try
//    {
//      final ICommand command = m_builder.finish();
//      if( command != null )
//      {
//        m_nodeTheme.getWorkspace().postCommand( command );
//        m_builder = new ElementGeometryBuilder( 4, m_nodeTheme );
//      }
//    }
//    catch( final Exception e )
//    {
//      e.printStackTrace();
//      KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
//    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    try
    {
      if(m_widgetStrategy!=null)
      {
        m_widgetStrategy.paint( g );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    
//    final Point currentPoint = m_currentPoint;
//
//    if( currentPoint != null )
//    {
//      if( m_builder != null )
//        m_builder.paint( g, getMapPanel().getProjection(), currentPoint );
//      g.drawRect( (int) currentPoint.getX() - 10, (int) currentPoint.getY() - 10, 20, 20 );
//    }
  }

  private  final void deleteCurrentSelection()
  {
    MapPanel mapPanel = getMapPanel();
    IFeatureSelectionManager selectionManager = 
                        mapPanel.getSelectionManager();
    EasyFeatureWrapper[] selected = 
        selectionManager.getAllFeatures();
    selectionManager.clear();
    if(selected.length==0)
    {
      return;
    }
    
    //feature are supposed to be in the same model
    Feature sampleFeature = selected[0].getFeature();
    Feature parentFeature = sampleFeature.getParent();
    IFEDiscretisationModel1d2d model1d2d=
        (IFEDiscretisationModel1d2d) parentFeature.getAdapter( IFEDiscretisationModel1d2d.class );
    if(model1d2d==null)
    {
      throw new RuntimeException("Could not found model1d2d");
    }    
    
    try
    {
      IKalypsoFeatureTheme featureTheme = UtilMap.findEditableTheme( 
          mapPanel.getMapModell(), 
          Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT );
      CommandableWorkspace workspace = featureTheme.getWorkspace();
      
      ChangeDiscretiationModelCommand modelChangeCmd=
                      new ChangeDiscretiationModelCommand(
                          workspace,
                          model1d2d);
//      for(EasyFeatureWrapper easyFeatureWrapper:selected)
//      {
//        try
//        {
//          Feature feature = easyFeatureWrapper.getFeature();
//          DeleteFE1D2DElement2DCmd cmd = 
//            new DeleteFE1D2DElement2DCmd(model1d2d,feature);
//          modelChangeCmd.addCommand( cmd );
//        }
//        catch(Throwable th)
//        {
//         th.printStackTrace(); 
//        }
//      }
      DeleteCmdFactory.createDeleteCmd( model1d2d, selected, modelChangeCmd );
      workspace.postCommand( modelChangeCmd );
      
      
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException(e.getMessage(), e );
    }
    
  }
  
  /**
   * Does also navigation through {@link MapKeyNavigator}
   * 
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( KeyEvent e )
  {
    if(e.getKeyChar()==KeyEvent.VK_DELETE)
    {
      deleteCurrentSelection();
    }
    
    try
    {
      if(m_widgetStrategy!=null)
      {
        m_widgetStrategy.keyPressed( e );
      }
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
    
    try
    {
      MapKeyNavigator.navigateOnKeyEvent( getMapPanel(), e, true );
    }
    catch(Throwable th)
    {
      th.printStackTrace();
    }    
  }
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( KeyEvent e )
  {
    try
    {
      if(m_widgetStrategy!=null)
      {
        m_widgetStrategy.keyReleased( e );
      }
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
  }
  
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#dragged(java.awt.Point)
   */
  @Override
  public void dragged( Point p )
  {
    try
    {
      if(m_widgetStrategy!=null)
      {
        m_widgetStrategy.dragged( p );
      }
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
    
    //TODO: check if this repaint is really necessary
    MapPanel panel = getMapPanel();
    if (panel != null)
      panel.repaint();

  }
  
  
  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
//    widgetFace.disposeControl();
  }
  
  public void setStrategy(IWidget widgetStrategy)
  {
    if(this.m_widgetStrategy==widgetStrategy)
    {
      return;
    }
    
    if(this.m_widgetStrategy!=null)
    {
      this.m_widgetStrategy.finish();
    }
    
    this.m_widgetStrategy = widgetStrategy;    
    widgetStrategy.activate( getCommandTarget(), getMapPanel() );
  }
  
  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    try
    {
      if( m_widgetStrategy !=null )
      {
        m_widgetStrategy.finish();
      }
    }
    catch( Throwable th )
    {
      th.printStackTrace();
    }
    
    super.finish();
    
  }
  
}
