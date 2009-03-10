/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 * 
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 * 
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.editor.mapeditor;

import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ComponentListener;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.swing.SwingUtilities;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.internal.ObjectActionContributorManager;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.contribs.eclipse.swt.events.SWTAWT_ContextMenuMouseAdapter;
import org.kalypso.contribs.eclipse.ui.forms.MessageUtilitites;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.listeners.MapPanelAdapter;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.editor.actions.FeatureActionUtilities;
import org.kalypso.ui.editor.gmleditor.ui.FeatureAssociationTypeElement;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;

/**
 * Helper class for {@link org.eclipse.ui.IWorkbenchPart}s, which show a map.
 * 
 * @author Gernot Belger
 */
@SuppressWarnings("restriction")
public class MapPartHelper
{
  final static ISchedulingRule UI_JOB_MUTEXT = new MutexRule();

  private MapPartHelper( )
  {
    // will not be instantiated
  }

  /**
   * Does nothing more than create an empty form containing a {@link FillLayout}'ed body. This form is suitable to be
   * filled next by {@link #createMapPanelInForm(Form, ICommandTarget, IFeatureSelectionManager).
   */
  public static Form createMapForm( final Composite parent )
  {
    final FormToolkit formToolkit = new FormToolkit( parent.getDisplay() );
    final Form form = formToolkit.createForm( parent );
    form.setSeparatorVisible( true );
    final Composite body = form.getBody();
    body.setLayout( new FillLayout() );
    return form;
  }

  /**
   * Creates a {@link MapPanel} on a form. The body's layout must already have been set.<br>
   * The form message will be reflecting the status of the {@link MapPanel}.
   * 
   * @see createMapForm
   */
  public static IMapPanel createMapPanelInForm( final Form form, final ICommandTarget commandTarget, final IFeatureSelectionManager selectionManager )
  {
    final IMapPanel mapPanel = createMapPanel( form.getBody(), SWT.NONE, null, commandTarget, selectionManager );

    mapPanel.addMapPanelListener( new MapPanelAdapter()
    {
      /**
       * @see org.kalypso.ogc.gml.map.listeners.MapPanelAdapter#onStatusChanged(org.kalypso.ogc.gml.map.IMapPanel)
       */
      @Override
      public void onStatusChanged( final IMapPanel source )
      {
        final IStatus status = source.getStatus();

        final UIJob job = new UIJob( "Update status" )
        {
          @Override
          public IStatus runInUIThread( final IProgressMonitor monitor )
          {
            if( form.isDisposed() )
              return Status.OK_STATUS;

            final String oldMessage = form.getMessage();
            final boolean ok = status.isOK();
            if( ok )
              form.setMessage( null );
            else
              MessageUtilitites.setMessage( form, status );
            // TODO: add hyperlink listener if sub-messages exist. Show these sub-messages on click
            // Same for tooltip support

            // If visibility of header changed, send resize event, else the map has not the right extent
            if( source instanceof ComponentListener && ((oldMessage == null && !ok) || (oldMessage != null && ok)) )
              ((ComponentListener) source).componentResized( null );
            return Status.OK_STATUS;
          }
        };
        job.setRule( UI_JOB_MUTEXT );
        job.setSystem( true );
        job.schedule();
      }
    } );

    return mapPanel;
  }

  public static IMapPanel createMapPanel( final Composite parent, final int style, final Object layoutData, final ICommandTarget commandTarget, final IFeatureSelectionManager selectionManager )
  {
    final MapPanel mapPanel = new MapPanel( commandTarget, selectionManager );
    final Composite mapComposite = new Composite( parent, style | SWT.EMBEDDED | SWT.NO_BACKGROUND );
    mapComposite.setLayoutData( layoutData );
    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );
    virtualFrame.add( mapPanel );

    // channel focus to awt
    mapComposite.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusGained(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusGained( final FocusEvent e )
      {
        SwingUtilities.invokeLater( new Runnable()
        {
          public void run( )
          {
            mapPanel.requestFocus();
          }
        } );
      }
    } );

// final MapCanvas mapPanel = new MapCanvas( parent, style, commandTarget, selectionManager );
// mapPanel.setLayoutData( layoutData );

    return mapPanel;
  }

  public static MenuManager createMapContextMenu( final Composite parent, final IMapPanel mapPanel, final IWorkbenchPartSite site )
  {
    // create Context Menu
    final MenuManager menuManager = new MenuManager();
    menuManager.setRemoveAllWhenShown( true );
    menuManager.addMenuListener( new IMenuListener()
    {
      public void menuAboutToShow( final IMenuManager manager )
      {
        fillMapContextMenu( site.getPart(), manager, mapPanel );
      }
    } );

    final Menu mapMenu = menuManager.createContextMenu( parent );
    parent.setMenu( mapMenu );
    // register it
    if( mapPanel instanceof Component )
      ((Component) mapPanel).addMouseListener( new SWTAWT_ContextMenuMouseAdapter( parent, mapMenu ) );

    return menuManager;
  }


  /**
   * Add some special actions to the menuManager, depending on the current selection.
   */
  public static void fillMapContextMenu( final IWorkbenchPart part, final IMenuManager manager, final IMapPanel mapPanel )
  {
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();

    /* Menu depending on active theme */
    final IKalypsoTheme activeTheme = mapPanel.getMapModell().getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      manager.add( new GroupMarker( "themeActions" ) ); //$NON-NLS-1$

      /* Add a 'new' menu corresponding to the theme's feature type. */
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) activeTheme;
      final ThemeFeatureSelection themeFeatureSelection = new ThemeFeatureSelection( theme );
      final IMenuManager newManager = FeatureActionUtilities.createFeatureNewMenu( themeFeatureSelection, selectionManager );
      manager.add( newManager );

      /* Also add specific theme actions. */
      final StructuredSelection themeSelection = new StructuredSelection( theme );
      final ISelectionProvider selectionProvider = new ISelectionProvider()
      {
        public void addSelectionChangedListener( final ISelectionChangedListener listener )
        {
        }

        public ISelection getSelection( )
        {
          return themeSelection;
        }

        public void removeSelectionChangedListener( final ISelectionChangedListener listener )
        {
        }

        public void setSelection( final ISelection selection )
        {
        }
      };
      final IMenuManager themeManager = new MenuManager( Messages.getString( "org.kalypso.ui.editor.mapeditor.MapPartHelper.2" ), "themeActions" ); //$NON-NLS-1$ //$NON-NLS-2$
      ObjectActionContributorManager.getManager().contributeObjectActions( part, themeManager, selectionProvider );
      manager.add( themeManager );
    }

    // add additions separator: if not, eclipse whines
    manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
  }

  private static class ThemeFeatureSelection implements IFeatureSelection
  {
    private final IKalypsoFeatureTheme m_theme;

    private final FeatureAssociationTypeElement m_fate;

    private final List<FeatureAssociationTypeElement> m_selection;

    public ThemeFeatureSelection( final IKalypsoFeatureTheme theme )
    {
      m_theme = theme;

      final FeatureList featureList = m_theme.getFeatureList();
      m_fate = new FeatureAssociationTypeElement( featureList.getParentFeature(), featureList.getParentFeatureTypeProperty() );
      m_selection = Collections.singletonList( m_fate );
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getAllFeatures()
     */
    public EasyFeatureWrapper[] getAllFeatures( )
    {
      return new EasyFeatureWrapper[] {};
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedFeature()
     */
    public Feature getFocusedFeature( )
    {
      return null;
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getFocusedProperty()
     */
    public IPropertyType getFocusedProperty( )
    {
      return null;
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getParentFeature(org.kalypsodeegree.model.feature.Feature)
     */
    public Feature getParentFeature( final Feature feature )
    {
      return m_fate.getParentFeature();
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getParentFeatureProperty(org.kalypsodeegree.model.feature.Feature)
     */
    public IRelationType getParentFeatureProperty( final Feature feature )
    {
      return m_fate.getAssociationTypeProperty();
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getSelectionManager()
     */
    public IFeatureSelectionManager getSelectionManager( )
    {
      return m_theme.getSelectionManager();
    }

    /**
     * @see org.kalypso.ogc.gml.selection.IFeatureSelection#getWorkspace(org.kalypsodeegree.model.feature.Feature)
     */
    public CommandableWorkspace getWorkspace( final Feature feature )
    {
      return m_theme.getWorkspace();
    }

    /**
     * @see org.eclipse.jface.viewers.IStructuredSelection#getFirstElement()
     */
    public Object getFirstElement( )
    {
      return m_fate;
    }

    /**
     * @see org.eclipse.jface.viewers.IStructuredSelection#iterator()
     */
    public Iterator<FeatureAssociationTypeElement> iterator( )
    {
      return m_selection.iterator();
    }

    /**
     * @see org.eclipse.jface.viewers.IStructuredSelection#size()
     */
    public int size( )
    {
      return 1;
    }

    /**
     * @see org.eclipse.jface.viewers.IStructuredSelection#toArray()
     */
    public Object[] toArray( )
    {
      return new Object[] { m_fate };
    }

    /**
     * @see org.eclipse.jface.viewers.IStructuredSelection#toList()
     */
    public List<FeatureAssociationTypeElement> toList( )
    {
      return m_selection;
    }

    /**
     * @see org.eclipse.jface.viewers.ISelection#isEmpty()
     */
    public boolean isEmpty( )
    {
      return false;
    }
  }
}
