package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.featureview.IFeatureChangeListener;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.table.celleditors.IFeatureModifierFactory;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.IGMLWorkspaceModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.FeaturePath;

/**
 * @author belger
 */
public class TableFeatureContol extends AbstractFeatureControl implements ModellEventListener
{
  private final IFeatureModifierFactory m_factory;

  private LayerTableViewer m_viewer;

  private KalypsoFeatureTheme m_kft;

  private final JobExclusiveCommandTarget m_target;

  protected Collection<ModifyListener> m_listeners = new ArrayList<ModifyListener>();

  private final IFeatureSelectionManager m_selectionManager;

  private final IFeatureChangeListener m_fcl;

  public TableFeatureContol( final IPropertyType ftp,
      final IFeatureModifierFactory factory, final IFeatureSelectionManager selectionManager, final IFeatureChangeListener fcl )
  {
    super( ftp );

    m_factory = factory;
    m_selectionManager = selectionManager;
    m_fcl = fcl;
    m_target = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_viewer = new LayerTableViewer( parent, SWT.NONE, m_target, m_factory, m_selectionManager, m_fcl );

    setFeature( getFeature() );

    /**/
    MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( IMenuManager manager )
      {
        manager.add( new GroupMarker( IWorkbenchActionConstants.MB_ADDITIONS ) );
        manager.add( new Separator() );
        //    mgr.add(selectAllAction);
      }
    } );

    final IWorkbenchWindow activeWorkbenchWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    final IWorkbenchPage activePage = activeWorkbenchWindow.getActivePage();
    final IEditorPart activeEditor = activePage.getActiveEditor();
    if( activeEditor != null )
    {
      m_viewer.setMenu( menuManager );
      // TODO check if we can register the menu more global, even when we have
      // no active editor
      activeEditor.getSite().registerContextMenu( menuManager, m_viewer );
    }
    return m_viewer.getControl();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#dispose()
   */
  @Override
  public void dispose()
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
      m_kft.dispose();
      m_kft = null;
    }

    final GMLWorkspace workspace = feature == null ? null : feature.getWorkspace();
    if( m_viewer != null && workspace != null && feature != null )
    {
      final FeaturePath parentFeaturePath = workspace.getFeaturepathForFeature( feature );
      final String ftpName = getFeatureTypeProperty().getName();
      final FeaturePath featurePath = new FeaturePath( parentFeaturePath, ftpName );

      final CommandableWorkspace c_workspace;
      if( workspace instanceof CommandableWorkspace )
        c_workspace = (CommandableWorkspace)workspace;
      else
        c_workspace = new CommandableWorkspace( workspace );

      m_kft = new KalypsoFeatureTheme( c_workspace, featurePath.toString(), ftpName, m_selectionManager );
      m_kft.addModellListener( this );
      m_viewer.setInput( m_kft );

      // create columns
      // add all columns: TODO: use template?
      final IFeatureType featureType = m_kft.getFeatureType();
      final IPropertyType[] properties = featureType == null ? new IPropertyType[0] : featureType.getProperties();
      for( int i = 0; i < properties.length; i++ )
      {
        final IPropertyType ftp = properties[i];
        m_viewer.addColumn( ftp.getQName().getLocalPart(), true, 100, "SWT.CENTER", null, i == properties.length - 1 );
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl()
  {
    m_viewer.refresh();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid()
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
    if( modellEvent != null )
    {
      if( modellEvent instanceof IGMLWorkspaceModellEvent
          && ( (IGMLWorkspaceModellEvent)modellEvent ).getGMLWorkspace() == m_kft.getWorkspace() )
      {
        final Event event = new Event();
        final Control control = m_viewer.getControl();
        if( control != null && !control.isDisposed() )
        {
          control.getDisplay().asyncExec( new Runnable()
          {
            public void run()
            {
              event.widget = control;
              final ModifyEvent me = new ModifyEvent( event );
              for( final Iterator mIt = m_listeners.iterator(); mIt.hasNext(); )
                ( (ModifyListener)mIt.next() ).modifyText( me );

            }
          } );
        }
      }
    }
  }
}