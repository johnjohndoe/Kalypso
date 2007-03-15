package org.kalypso.kalypsomodel1d2d.ui.map.del;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
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
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Provide widget for deleting finit elements 
 * 
 * @author Patrice Congo
 */
public class DeleteFEElementsWidget extends AbstractWidget implements /*IWidgetWithOptions,*/ WidgetStrategyContext
{

//  private DeleteFEElementsWidgetFace widgetFace=
//                      new DeleteFEElementsWidgetFace(this);

  private IWidget widgetStrategy = new FeElementPointSelectionWidget();
  
  
  public DeleteFEElementsWidget( )
  {
    super( "Delete Finite element", "Delete finite element" );
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
    if(widgetStrategy!=null)
    {
      widgetStrategy.activate( commandPoster, mapPanel );
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
      if(widgetStrategy!=null)
      {
        widgetStrategy.moved( p );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#leftClicked(java.awt.Point)
   */
  @Override
  public void leftClicked( final Point p )
  {
    try
    {
      if(widgetStrategy!=null)
      {
        widgetStrategy.leftClicked( p );
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
      if(widgetStrategy!=null)
      {
        widgetStrategy.leftPressed( p );
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
      if(widgetStrategy!=null)
      {
        widgetStrategy.leftReleased( p );
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
      if(widgetStrategy!=null)
      {
        widgetStrategy.doubleClickedLeft( p );
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
      if(widgetStrategy!=null)
      {
        widgetStrategy.paint( g );
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
      IKalypsoFeatureTheme featureTheme = UtilMap.findEditableThem( 
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
    }
    
  }
  
  /**
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
      if(widgetStrategy!=null)
      {
        widgetStrategy.keyPressed( e );
      }
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
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
      if(widgetStrategy!=null)
      {
        widgetStrategy.keyReleased( e );
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
      if(widgetStrategy!=null)
      {
        widgetStrategy.dragged( p );
      }
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }
  }
  
  
  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
//    return widgetFace.createControl( parent );
    return null;
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
    if(this.widgetStrategy==widgetStrategy)
    {
      return;
    }
    
    if(this.widgetStrategy!=null)
    {
      this.widgetStrategy.finish();
    }
    
    this.widgetStrategy = widgetStrategy;    
    widgetStrategy.activate( getCommandTarget(), getMapPanel() );
    
  }
  
}
