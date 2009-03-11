/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.outline;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang.NotImplementedException;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerUtilities;
import org.kalypso.ogc.gml.AbstractKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.RuleTreeObject;
import org.kalypso.ogc.gml.UserStyleTreeObject;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModellListener;
import org.kalypso.ogc.gml.mapmodel.MapModellAdapter;

/**
 * Content provider for modifying the outline tree. It filters the styles and the rules.
 *
 * @author Holger Albert
 */
public class GisMapOutlineContentProvider extends BaseWorkbenchContentProvider
{
  private final IMapModellListener m_modelListener = new MapModellAdapter()
  {

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeActivated(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeActivated( final IMapModell source, final IKalypsoTheme previouslyActive, final IKalypsoTheme nowActive )
    {
      final GisMapOutlineLabelProvider labelProvider = getLabelProvider();
      final TreeViewer viewer = getViewer();

      if( viewer != null && !viewer.getControl().isDisposed() )
      {
        viewer.getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            if( previouslyActive != null && nowActive != null )
              labelProvider.elementsChanged( previouslyActive, nowActive );
            else if( previouslyActive != null )
              labelProvider.elementsChanged( previouslyActive );
            else if( nowActive != null )
              labelProvider.elementsChanged( nowActive );
          }
        } );
      }
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeAdded(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeAdded( final IMapModell source, final IKalypsoTheme theme )
    {
      final TreeViewer viewer = getViewer();
      if( viewer != null && !viewer.getControl().isDisposed() )
      {
        viewer.getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            if( !viewer.getControl().isDisposed() )
            {
              viewer.refresh();
              if( viewer instanceof CheckboxTreeViewer )
                resetCheckState( theme );
            }
          }
        } );
      }
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeOrderChanged(org.kalypso.ogc.gml.mapmodel.IMapModell)
     */
    @Override
    public void themeOrderChanged( final IMapModell source )
    {
      final Viewer viewer = getViewer();
      ViewerUtilities.refresh( viewer, true );
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeRemoved(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @Override
    public void themeRemoved( final IMapModell source, final IKalypsoTheme theme, final boolean lastVisibility )
    {
      final TreeViewer viewer = getViewer();
      if( viewer != null && !viewer.getControl().isDisposed() )
      {
        viewer.getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            if( !viewer.getControl().isDisposed() )
              viewer.remove( theme );
          }
        } );
      }
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.MapModellAdapter#themeVisibilityChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme, boolean)
     */
    @Override
    public void themeVisibilityChanged( final IMapModell source, final IKalypsoTheme theme, final boolean visibility )
    {
      final Viewer viewer = getViewer();
      if( viewer instanceof CheckboxTreeViewer && !viewer.getControl().isDisposed() )
      {
        viewer.getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            if( !viewer.getControl().isDisposed() )
              ((CheckboxTreeViewer) viewer).setChecked( theme, visibility );
          }
        } );
      }
    }

    /**
     * @see org.kalypso.ogc.gml.mapmodel.IMapModellListener#themeStatusChanged(org.kalypso.ogc.gml.mapmodel.IMapModell,
     *      org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void themeStatusChanged( final IMapModell source, final IKalypsoTheme theme )
    {
      final TreeViewer viewer = getViewer();
      ViewerUtilities.refresh( viewer, theme, true );

      if( viewer != null && !viewer.getControl().isDisposed() )
      {
        viewer.getControl().getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            if( !viewer.getControl().isDisposed() )
              viewer.refresh( theme );
          }
        } );
      }
    }

  };

  private TreeViewer m_viewer;

  private final GisMapOutlineLabelProvider m_labelProvider;

  public GisMapOutlineContentProvider( final GisMapOutlineLabelProvider labelProvider )
  {
    m_labelProvider = labelProvider;
  }

  protected TreeViewer getViewer( )
  {
    return m_viewer;
  }

  protected GisMapOutlineLabelProvider getLabelProvider( )
  {
    return m_labelProvider;
  }

  /**
   * @see org.eclipse.ui.model.BaseWorkbenchContentProvider#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object element )
  {
    /* A moment please, if the theme is configured, not to show its children, then ignore them. */
    // TODO: should configurable for every level
    if( element instanceof AbstractKalypsoTheme )
    {
      final AbstractKalypsoTheme theme = (AbstractKalypsoTheme) element;
      if( theme.shouldShowLegendChildren() == false )
        return new Object[] {};
    }

    // FIXME: remove mode 2 (if mode 1 is thoroughly tested); make 1 or 2 a theme-property or at least a flag in this
    // content provider
    final int compactifyMode = 1;
    switch( compactifyMode )
    {
      case 0: // none
        return super.getChildren( element );

      case 1: // total
        return compactChildren( element );

      case 2: // old modus
        return compactChildrenOld( element );

      default:
        throw new NotImplementedException();
    }

  }

  private Object[] compactChildren( final Object element )
  {
    final Object[] children = super.getChildren( element );

    // Do not compactify the model itself; maybe introduce a common interface
    // that decided, if compactification is allowed
    if( element instanceof IMapModell )
      return children;

    /* Normal case none or more than one children */
    if( children == null || children.length != 1 )
      return children;

    return getChildren( children[0] );
  }

  private Object[] compactChildrenOld( final Object element )
  {
    final Object[] children = super.getChildren( element );

    /* If no children are there, return the result. */
    if( children == null || children.length == 0 )
      return children;

    /* If more then one child are there, return the result. */
    if( children.length > 1 )
      return children;

    /* Now there is only one child. If it is not a style or a rule, return the result. */
    if( !(children instanceof UserStyleTreeObject[]) && !(children instanceof RuleTreeObject[]) )
      return children;

    /* It has to be a IWorkbenchAdapter. */
    final IWorkbenchAdapter child = (IWorkbenchAdapter) children[0];

    /* Get the children of the child. */
    final Object[] children2 = super.getChildren( child );

    /* If there are more then one as a result, return them instead. */
    if( children == null )
      return new Object[] {};

    if( children2.length == 1 )
    {
      final Object subchild = children2[0];
      if( subchild instanceof IWorkbenchAdapter )
        return ((IWorkbenchAdapter) subchild).getChildren( subchild );
    }
    else if( children2.length > 1 )
    {
      if( areRuleTreeObjects( children2 ) )
        return getUniqueRuleLabels( children2 );
      else
        return children2;
    }

    /* Otherwise ignore it. */
    return new Object[] {};
  }

  private Object[] getUniqueRuleLabels( final Object[] children )
  {
    // sort out obsolete rule labels
    final List<Object> rules = new ArrayList<Object>();
    final Set<String> rulenames = new HashSet<String>();

    for( final Object object : children )
    {
      final RuleTreeObject rule = (RuleTreeObject) object;

      final String name = rule.getRule().getName();
      if( rulenames.contains( name ) )
      {
        continue;
      }

      rulenames.add( name );
      rules.add( rule );
    }

    return rules.toArray();
  }

  private boolean areRuleTreeObjects( final Object[] children )
  {
    for( final Object object : children )
    {
      if( !(object instanceof RuleTreeObject) )
        return false;
    }

    return true;
  }

  /**
   * @see org.eclipse.ui.model.BaseWorkbenchContentProvider#getParent(java.lang.Object)
   */
  @Override
  public Object getParent( final Object element )
  {
    /* Only rules may jump above parents. */
    if( !(element instanceof RuleTreeObject) )
      return super.getParent( element );

    /* Get the parent for the rule (this would be the style). */
    final Object style = super.getParent( element );

    /* Get the parent of the style (this would be a theme). */
    final Object theme = super.getParent( style );

    /* Get the children of the theme (this would be all styles). */
    final Object[] styles = getChildren( theme );

    /* Check, if there are more than one style. If so, return the normal result. */
    if( styles.length > 1 )
      return style;

    /* Otherwise, there is only one style. */
    return theme;
  }

  /**
   * @see org.eclipse.ui.model.WorkbenchContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_viewer = (TreeViewer) viewer;

    if( oldInput instanceof IMapModell )
      ((IMapModell) oldInput).removeMapModelListener( m_modelListener );

    if( newInput instanceof IMapModell )
      ((IMapModell) newInput).addMapModelListener( m_modelListener );

    m_viewer.addTreeListener( new ITreeViewerListener()
    {
      /**
       * @see org.eclipse.jface.viewers.ITreeViewerListener#treeCollapsed(org.eclipse.jface.viewers.TreeExpansionEvent)
       */
      public void treeCollapsed( final TreeExpansionEvent event )
      {
      }

      /**
       * @see org.eclipse.jface.viewers.ITreeViewerListener#treeExpanded(org.eclipse.jface.viewers.TreeExpansionEvent)
       */
      public void treeExpanded( final TreeExpansionEvent event )
      {
        resetCheckState( newInput );
      }
    } );

    // Reset check state later, else it will not work, as no children have been created yet
    m_viewer.getControl().getDisplay().asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        resetCheckState( newInput );
      }
    } );

  }

  protected void resetCheckState( final Object object )
  {
    if( object == null || !(m_viewer instanceof CheckboxTreeViewer) )
      return;

    final CheckboxTreeViewer viewer = (CheckboxTreeViewer) m_viewer;

    final boolean checked = m_labelProvider.isChecked( object );
    final boolean grayed = m_labelProvider.isGrayed( object );

    viewer.setGrayed( object, grayed );
    viewer.setChecked( object, grayed || checked );

    final Object[] children = getChildren( object );
    for( final Object child : children )
      resetCheckState( child );
  }

}