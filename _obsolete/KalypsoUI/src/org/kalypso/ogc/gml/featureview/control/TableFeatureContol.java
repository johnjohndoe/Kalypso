package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.core.util.pool.KeyInfo;
import org.kalypso.core.util.pool.ResourcePool;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.KalypsoTableFeatureTheme;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.editor.actions.FeatureActionUtilities;
import org.kalypso.ui.editor.actions.TableFeatureControlUtils;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.IGMLWorkspaceModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

/**
 * @author Gernot Belger
 */
public class TableFeatureContol extends AbstractFeatureControl implements ModellEventListener
{
  private final IFeatureModifierFactory m_factory;

  protected LayerTableViewer m_viewer;

  protected KalypsoTableFeatureTheme m_kft;

  private final JobExclusiveCommandTarget m_target;

  protected Collection<ModifyListener> m_listeners = new ArrayList<ModifyListener>();

  protected final IFeatureSelectionManager m_selectionManager;

  private final IFeatureChangeListener m_fcl;

  private Gistableview m_tableView;

  private final boolean m_showToolbar;

  private final boolean m_showContextMenu;

  private ToolBarManager m_toolbarManager;

  public TableFeatureContol( final IPropertyType ftp, final IFeatureModifierFactory factory, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl, final boolean showToolbar, final boolean showContextMenu )
  {
    super( ftp );

    m_factory = factory;
    m_selectionManager = selectionManager;
    m_fcl = fcl;
    m_target = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );
    m_showToolbar = showToolbar;
    m_showContextMenu = showContextMenu;
    m_toolbarManager = null;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    /* Create a new Composite for the toolbar. */
    final Composite client = new Composite( parent, style );
    if( m_showToolbar )
      client.setLayout( new GridLayout( 2, false ) );
    else
      client.setLayout( new GridLayout( 1, false ) );

    /* Create the layer table viewer. */
    m_viewer = new LayerTableViewer( client, SWT.NONE, m_target, m_factory, m_selectionManager, m_fcl );
    m_viewer.getTable().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    /* Set the feature. */
    final Feature feature = getFeature();
    setFeature( feature );

    /* If wanted, add a toolbar. */
    if( m_showToolbar )
    {
      /* Create the toolbar manager. */
      m_toolbarManager = new ToolBarManager( SWT.VERTICAL );

      /* IAction for adding a feature. */

      // TODO: consider the case, where multiple feature-types substitute the target feature type
      final IRelationType parentRelation = (IRelationType) getFeatureTypeProperty();
      final String actionLabel = parentRelation == null ? Messages.getString("org.kalypso.ogc.gml.featureview.control.TableFeatureContol.0") : FeatureActionUtilities.newFeatureActionLabel( parentRelation.getTargetFeatureType() ); //$NON-NLS-1$
      final IAction addAction = new Action( actionLabel + Messages.getString("org.kalypso.ogc.gml.featureview.control.TableFeatureContol.1"), ImageProvider.IMAGE_FEATURE_NEW ) //$NON-NLS-1$
      {
        /**
         * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
         */
        @Override
        public void runWithEvent( final Event event )
        {
          if( checkMaxCount() == false )
          {
            final Shell shell = event.display.getActiveShell();
            MessageDialog.openInformation( shell, Messages.getString("org.kalypso.ogc.gml.featureview.control.TableFeatureContol.2"), Messages.getString("org.kalypso.ogc.gml.featureview.control.TableFeatureContol.3") ); //$NON-NLS-1$ //$NON-NLS-2$
            return;
          }

          /* Get the needed properties. */
          final Feature parentFeature = getFeature();
          final CommandableWorkspace workspace = m_kft.getWorkspace();

          final AddFeatureCommand command = new AddFeatureCommand( workspace, parentRelation.getTargetFeatureType(), parentFeature, parentRelation, -1, null, null, 0 );
          fireFeatureChange( command );
        }

        /**
         * This function checks, if more features can be added.
         *
         * @return True, if so.
         */
        private boolean checkMaxCount( )
        {
          int maxOccurs = -1;
          int size = -1;

          /* Get the needed properties. */
          final Feature parentFeature = getFeature();

          maxOccurs = parentRelation.getMaxOccurs();
          if( parentFeature instanceof List )
          {
            size = ((List< ? >) parentFeature).size();
            if( maxOccurs == IPropertyType.UNBOUND_OCCURENCY )
              return true;
            else if( maxOccurs < size )
              return false;
          }

          return true;
        }
      };

      /* Add the new Item. */
      m_toolbarManager.add( addAction );

      /* IAction for removing a feature. */
      final IAction removeAction = new Action( actionLabel + Messages.getString("org.kalypso.ogc.gml.featureview.control.TableFeatureContol.4"), ImageProvider.IMAGE_FEATURE_DELETE ) //$NON-NLS-1$
      {
        /**
         * @see org.eclipse.jface.action.Action#runWithEvent(org.eclipse.swt.widgets.Event)
         */
        @Override
        public void runWithEvent( final Event event )
        {
          if( canDelete() == false )
          {
            final Shell shell = event.display.getActiveShell();
            MessageDialog.openInformation( shell, actionLabel + Messages.getString("org.kalypso.ogc.gml.featureview.control.TableFeatureContol.5"), Messages.getString("org.kalypso.ogc.gml.featureview.control.TableFeatureContol.6") ); //$NON-NLS-1$ //$NON-NLS-2$
            return;
          }

          /* Get the shell. */
          final Shell shell = event.display.getActiveShell();

          /* Get the current selection. */
          final ISelection selection = m_viewer.getSelection();
          if( selection == null || !(selection instanceof IFeatureSelection) )
            return;

          /* Get all selected features. */
          final EasyFeatureWrapper[] allFeatures = ((IFeatureSelection) selection).getAllFeatures();

          /* Build the delete command. */
          final DeleteFeatureCommand command = TableFeatureControlUtils.deleteFeaturesFromSelection( allFeatures, shell );
          if( command != null )
          {
            /* Execute the command. */
            fireFeatureChange( command );

            /* Reset the selection. */
            m_viewer.setSelection( new StructuredSelection() );
          }
        }

        /**
         * This function checks, if there are features, which can be deleted.
         *
         * @return True, if so.
         */
        public boolean canDelete( )
        {
          final ISelection selection = m_viewer.getSelection();
          if( selection == null )
            return false;

          if( !(selection instanceof IFeatureSelection) )
            return false;

          final int featureCount = FeatureSelectionHelper.getFeatureCount( (IFeatureSelection) selection );
          if( featureCount > 0 )
            return true;

          return false;
        }
      };

      /* Add the new Item. */
      m_toolbarManager.add( removeAction );

      /* Create the toolbar. */
      final ToolBar toolbar = m_toolbarManager.createControl( client );
      toolbar.setLayoutData( new GridData( GridData.CENTER, GridData.BEGINNING, false, true ) );
    }

    /* Only show the context menu, if it is wanted to be shown. */
    if( m_showContextMenu )
    {
      /* Need a menu manager for the context menu. */
      final MenuManager menuManager = new MenuManager();
      menuManager.setRemoveAllWhenShown( true );
      menuManager.addMenuListener( new IMenuListener()
      {
        public void menuAboutToShow( final IMenuManager manager )
        {
          manager.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
          manager.add( new Separator() );
        }
      } );

      final IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
      final IWorkbenchPage activePage = activeWorkbenchWindow.getActivePage();
      final IWorkbenchPart activeEditor = activePage.getActivePart();
      if( activeEditor != null )
      {
        /* Set the context menu. */
        m_viewer.setMenu( menuManager );

        /* TODO Check if we can register the menu more global, even when we have no active editor. */
        activeEditor.getSite().registerContextMenu( menuManager, m_viewer );
      }
    }

    return client;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_viewer != null )
      m_viewer.dispose();

    if( m_kft != null )
      m_kft.dispose();

    m_target.dispose();

    super.dispose();

  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#setFeature(org.kalypsodeegree.model.feature.GMLWorkspace,
   *      org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public void setFeature( final Feature feature )
  {
    super.setFeature( feature );

    if( m_kft != null )
    {
      final CommandableWorkspace workspace = m_kft.getWorkspace();
      if( workspace != null )
        workspace.removeModellListener( this );
      m_kft.dispose();
      m_kft = null;
    }

    final GMLWorkspace workspace = feature == null ? null : feature.getWorkspace();
    if( m_viewer != null && workspace != null && feature != null )
    {
      final FeaturePath parentFeaturePath = workspace.getFeaturepathForFeature( feature );
      final String ftpName = getFeatureTypeProperty().getQName().getLocalPart();
      final FeaturePath featurePath = new FeaturePath( parentFeaturePath, ftpName );

      final CommandableWorkspace c_workspace = findCommandableWorkspace( workspace );

      m_kft = new KalypsoTableFeatureTheme( c_workspace, featurePath.toString(), new I10nString( ftpName ), m_selectionManager );

      c_workspace.addModellListener( this );
      m_viewer.setInput( m_kft );

      // create columns
      // add all columns
      if( m_tableView != null )
      {
        m_viewer.applyTableTemplate( m_tableView, workspace.getContext(), false );
      }
      else
      {
        final IFeatureType featureType = m_kft.getFeatureType();
        final IPropertyType[] properties = featureType == null ? new IPropertyType[0] : featureType.getProperties();
        for( int i = 0; i < properties.length; i++ )
        {
          final IPropertyType ftp = properties[i];
          m_viewer.addColumn( ftp.getQName().getLocalPart(), null, null, true, 100, "SWT.CENTER", null, i == properties.length - 1 ); //$NON-NLS-1$
        }
      }
    }
  }

  /**
   * Helps to find the right commandable workspace for the given feature.
   */
  private CommandableWorkspace findCommandableWorkspace( final GMLWorkspace workspace )
  {
    final ResourcePool pool = KalypsoCorePlugin.getDefault().getPool();
    final KeyInfo[] infos = pool.getInfos();
    for( final KeyInfo keyInfo : infos )
    {
      final Object object = keyInfo.getObject();
      if( object instanceof CommandableWorkspace && ((CommandableWorkspace) object).getWorkspace() == workspace )
        return (CommandableWorkspace) object;
    }

    final CommandableWorkspace c_workspace;
    if( workspace instanceof CommandableWorkspace )
      c_workspace = (CommandableWorkspace) workspace;
    else
      c_workspace = new CommandableWorkspace( workspace );
    return c_workspace;
  }

  public void setTableTemplate( final Gistableview tableView )
  {
    m_tableView = tableView;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    m_viewer.refresh();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_listeners.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_listeners.remove( l );
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent instanceof IGMLWorkspaceModellEvent && ((IGMLWorkspaceModellEvent) modellEvent).getGMLWorkspace() == m_kft.getWorkspace() )
    {
      final Event event = new Event();
      final Control control = m_viewer.getControl();
      if( control != null && !control.isDisposed() )
      {
        control.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            event.widget = control;
            final ModifyEvent me = new ModifyEvent( event );
            for( final Object element : m_listeners )
              ((ModifyListener) element).modifyText( me );

          }
        } );

// if( modellEvent instanceof FeatureChangeModellEvent )
// {
// final FeatureChangeModellEvent featureEvent = (FeatureChangeModellEvent) modellEvent;
// fireFeatureChange( featureEvent.getChanges() );
// }
      }
    }
  }

}