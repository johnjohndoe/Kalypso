package org.kalypso.kalypsomodel1d2d.ui.map.del;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteElement1DCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeletePolyElementCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENode;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Provide widget for deleting finite elements
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
      return;

    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( "Objekte löschen", "Selektierte Objekte werden gelöscht. Sind Sie sicher?" ) )
      return;

    selectionManager.clear();

    try
    {
      // to be allowed to delete the 2D element, no continuity line cannot be positioned on that element
      // to be allowed to delete the 1D element, that element cannot be the last one that touches some continuity line

      final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();
      final IFEDiscretisationModel1d2d discretisationModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class );

      if( discretisationModel == null )
        throw new RuntimeException( "Could not found model1d2d" );

      // make a list of all the nodes contained by all the continuity lines
      final List<IFENode> clNodes = new ArrayList<IFENode>();
      final IFeatureWrapperCollection<IFELine> continuityLines = discretisationModel.getContinuityLines();
      for( final IFELine line : continuityLines )
        clNodes.addAll( line.getNodes() ); // usually lines are not overlapped so there is no need to check if some of
      // the nodes are already in the list

      // 2D: check if any of the selected elements have nodes that belongs to any continuity line; if so, deleting is
      // not allowed
      // 1D: check if any of the selected elements have nodes that belongs to any continuity line; if that element is
      // the last one on the line, deleting is not allowed
      for( final EasyFeatureWrapper easyFeatureWrapper : selected )
      {
        if( easyFeatureWrapper == null )
          throw new IllegalArgumentException( "All easy features in selected must be non null" );
        final IFE1D2DElement element = (IFE1D2DElement) easyFeatureWrapper.getFeature().getAdapter( IFE1D2DElement.class );
        final List<IFE1D2DNode> nodes = element.getNodes();
        for( final IFE1D2DNode node : nodes )
          if( clNodes.contains( node ) )
          {
            if( element instanceof IElement2D )
            {
              SWT_AWT_Utilities.showSwtMessageBoxInformation( "Deletion forbidden", "At least one of the selected 2D elements is positioned at the continuity line. Please delete the continuity line before deleting such 2D element(s)." );
              // selectionManager.clear();
              return;
            }
            if( element instanceof IElement1D )
            {
              final IFeatureWrapperCollection containers = node.getContainers();
              int numberOfEdgeContainers = 0;
              for( final Object container : containers )
                if( container instanceof IFE1D2DEdge ) // container can be also a line
                  numberOfEdgeContainers++;
              if( numberOfEdgeContainers < 2 )
              {
                SWT_AWT_Utilities.showSwtMessageBoxInformation( "Deletion forbidden", "At least one of the selected 1D elements is positioned as the last 1D element that touches the continuity line. Please either delete the continuity line before deleting such 1D element(s) or connect the continuity line to another 1D element(s)." );
                // selectionManager.clear();
                return;
              }
            }
          }
      }

      final IKalypsoFeatureTheme featureTheme = UtilMap.findEditableTheme( mapPanel.getMapModell(), IFE1D2DElement.QNAME );
      final Set<Feature> changedFeatureList = new HashSet<Feature>();

      for( final EasyFeatureWrapper easyFeatureWrapper : selected )
      {
        final Feature feature = easyFeatureWrapper.getFeature();
        if( feature != null )
        {
          final IDiscrModel1d2dChangeCommand deleteCmd = DeleteCmdFactory.createDeleteCmd( feature, discretisationModel );
          // command.addCommand( deleteCmd );

          final CommandableWorkspace workspace = featureTheme.getWorkspace();
          workspace.postCommand( deleteCmd );

          if( deleteCmd instanceof DeletePolyElementCmd )
            changedFeatureList.addAll( ((DeletePolyElementCmd) deleteCmd).getChangedFeatureList() );
          else if( deleteCmd instanceof DeleteElement1DCmd )
            changedFeatureList.addAll( ((DeleteElement1DCmd) deleteCmd).getChangedFeatureList() );
        }
      }

      Feature distFeature = discretisationModel.getFeature();

      Feature[] deletedFeatures = changedFeatureList.toArray( new Feature[changedFeatureList.size()] );
      final GMLWorkspace discWorkspace = discretisationModel.getFeature().getWorkspace();
      FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( discWorkspace, distFeature, deletedFeatures, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
      discWorkspace.fireModellEvent( event );
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
