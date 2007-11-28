package org.kalypso.kalypsomodel1d2d.ui.map.del;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
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
    if( m_widgetStrategy != null )
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
      if( m_widgetStrategy != null )
      {
        m_widgetStrategy.moved( p );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
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
    try
    {
      if( m_widgetStrategy != null )
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
  public void leftPressed( final Point p )
  {
    try
    {
      if( m_widgetStrategy != null )
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
  public void leftReleased( final Point p )
  {
    try
    {
      if( m_widgetStrategy != null )
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
      if( m_widgetStrategy != null )
      {
        m_widgetStrategy.doubleClickedLeft( p );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    try
    {
      if( m_widgetStrategy != null )
      {
        m_widgetStrategy.paint( g );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private final void deleteCurrentSelection( )
  {
    final MapPanel mapPanel = getMapPanel();
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    final EasyFeatureWrapper[] selected = selectionManager.getAllFeatures();
    if( selected.length == 0 )
    {
      return;
    }

    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( "Objekte löschen", "Selektierte Objekte werden gelöscht. Sind Sie sicher?" ) )
      return;

    selectionManager.clear();

    // feature are supposed to be in the same model
    final Feature sampleFeature = selected[0].getFeature();
    final Feature parentFeature = sampleFeature.getParent();
    final IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d) parentFeature.getAdapter( IFEDiscretisationModel1d2d.class );
    if( model1d2d == null )
      throw new RuntimeException( "Could not found model1d2d" );

    try
    {
      final IKalypsoFeatureTheme featureTheme = UtilMap.findEditableTheme( mapPanel.getMapModell(), Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT );
      final CommandableWorkspace workspace = featureTheme.getWorkspace();

      final ChangeDiscretiationModelCommand modelChangeCmd = new ChangeDiscretiationModelCommand( workspace, model1d2d );
      DeleteCmdFactory.createDeleteCmd( model1d2d, selected, modelChangeCmd );
      workspace.postCommand( modelChangeCmd );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new RuntimeException( e.getMessage(), e );
    }
  }

  /**
   * Does also navigation through {@link MapKeyNavigator}
   * 
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyChar() == KeyEvent.VK_DELETE )
    {
      deleteCurrentSelection();
    }

    try
    {
      if( m_widgetStrategy != null )
      {
        m_widgetStrategy.keyPressed( e );
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
  public void keyReleased( final KeyEvent e )
  {
    try
    {
      if( m_widgetStrategy != null )
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
  public void dragged( final Point p )
  {
    try
    {
      if( m_widgetStrategy != null )
      {
        m_widgetStrategy.dragged( p );
      }
    }
    catch( final Exception ex )
    {
      ex.printStackTrace();
    }

    // TODO: check if this repaint is really necessary
    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();

  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    // widgetFace.disposeControl();
  }

  public void setStrategy( final IWidget widgetStrategy )
  {
    if( this.m_widgetStrategy == widgetStrategy )
    {
      return;
    }

    if( this.m_widgetStrategy != null )
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
      if( m_widgetStrategy != null )
      {
        m_widgetStrategy.finish();
      }
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
    }

    super.finish();

  }

}
