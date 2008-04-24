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

import java.awt.Frame;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import javax.swing.SwingUtilities;

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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.internal.ObjectActionContributorManager;
import org.kalypso.contribs.eclipse.swt.events.SWTAWT_ContextMenuMouseAdapter;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
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
  private MapPartHelper( )
  {
    // will not be instantiated
  }

  public static Control createMapPanelPartControl( final Composite parent, final MapPanel mapPanel, final IWorkbenchPartSite site, boolean doCreateMenu )
  {
    final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED | SWT.NO_BACKGROUND );
    // create MapPanel
    final Frame virtualFrame = SWT_AWT.new_Frame( composite );
    virtualFrame.setVisible( true );
    mapPanel.setVisible( true );
    virtualFrame.add( mapPanel );

    // channel focus to awt
    composite.addFocusListener( new FocusAdapter()
    {
      /**
       * @see org.eclipse.swt.events.FocusAdapter#focusGained(org.eclipse.swt.events.FocusEvent)
       */
      @Override
      public void focusGained( FocusEvent e )
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

    // create Context Menu
    if( doCreateMenu )
    {
      final MenuManager menuManager = new MenuManager();
      menuManager.setRemoveAllWhenShown( true );
      menuManager.addMenuListener( new IMenuListener()
      {
        public void menuAboutToShow( final IMenuManager manager )
        {
          handleMenuAboutToShow( site.getPart(), manager, mapPanel );
        }
      } );

      final Menu mapMenu = menuManager.createContextMenu( composite );
      composite.setMenu( mapMenu );
      // register it
      site.registerContextMenu( menuManager, mapPanel );
      mapPanel.addMouseListener( new SWTAWT_ContextMenuMouseAdapter( composite, mapMenu ) );
    }

    site.setSelectionProvider( mapPanel );

    return composite;
  }

  /**
   * Add some special actions to the menuManager, dependend on the current selection.
   */
  public static void handleMenuAboutToShow( final IWorkbenchPart part, final IMenuManager manager, final MapPanel mapPanel )
  {
    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();

    /* Menu depending on active theme */
    final IKalypsoTheme activeTheme = mapPanel.getMapModell().getActiveTheme();
    if( activeTheme instanceof IKalypsoFeatureTheme )
    {
      manager.add( new GroupMarker( "themeActions" ) );

      /* Add a 'new' menu corresponding to the theme's feature type. */
      final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) activeTheme;
      final ThemeFeatureSelection themeFeatureSelection = new ThemeFeatureSelection( theme );
      final IMenuManager newManager = FeatureActionUtilities.createFeatureNewMenu( themeFeatureSelection, selectionManager );
      manager.add( newManager );

      /* Also add specific theme actions. */
      final StructuredSelection themeSelection = new StructuredSelection( theme );
      final ISelectionProvider selectionProvider = new ISelectionProvider()
      {
        public void addSelectionChangedListener( ISelectionChangedListener listener )
        {
        }

        public ISelection getSelection( )
        {
          return themeSelection;
        }

        public void removeSelectionChangedListener( ISelectionChangedListener listener )
        {
        }

        public void setSelection( ISelection selection )
        {
        }
      };
      final IMenuManager themeManager = new MenuManager( "&Thema", "themeActions" );
      ObjectActionContributorManager.getManager().contributeObjectActions( part, themeManager, selectionProvider );
      manager.add( themeManager );
    }

    // add additions seperator: if not, eclipse whines
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
    public Iterator iterator( )
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
    public List toList( )
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
