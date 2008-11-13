/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 *
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 *
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 *
 * and
 *
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * Contact:
 *
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 *
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ui.editor.gmleditor.ui;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Control;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.catalogs.FeatureTypePropertiesCatalog;
import org.kalypso.ui.catalogs.IFeatureTypePropertiesConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPath;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathException;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathSegment;
import org.kalypsodeegree_impl.model.feature.gmlxpath.GMLXPathUtilities;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author Christoph Küpferle
 * @author Gernot Belger
 */
public class GMLContentProvider implements ITreeContentProvider
{
  private final ModellEventListener m_workspaceListener = new ModellEventListener()
  {
    public void onModellChange( final ModellEvent modellEvent )
    {
      handleModelChanged( modellEvent );
    }
  };

  private TreeViewer m_viewer;

  private GMLWorkspace m_workspace;

  /**
   * The x-path to the currently (not visible) root element. Its children are the root elements of the tree.
   * <p>
   * If null, the root-feature is the root element of the tree.
   */
  private GMLXPath m_rootPath = new GMLXPath( "", null ); //$NON-NLS-1$

  private final boolean m_showAssociations;

  private final boolean m_handleModelEvents;

  /**
   * @deprecated Use the full constructor instead
   */
  @Deprecated
  public GMLContentProvider( )
  {
    this( true, false );
  }

  public GMLContentProvider( final boolean showAssociations )
  {
    this( showAssociations, true );
  }

  /**
   * Full construcotr.<br>
   * Likely to be replaced by {@link #GMLContentProvider(boolean)} in the future.<br>
   *
   * @param handleModelEvents
   *            Only for backwards compability. Should always be set to <code>true</code>.
   */
  public GMLContentProvider( final boolean showAssociations, final boolean handleModelEvents )
  {
    m_showAssociations = showAssociations;
    m_handleModelEvents = handleModelEvents;
  }

  /**
   * Gets the children and updates the parent-hash.
   *
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
    if( m_workspace == null )
      return new Object[] {};

    // Test first if we should show children
    final QName qname;
    if( parentElement instanceof Feature )
      qname = ((Feature) parentElement).getFeatureType().getQName();
    else if( parentElement instanceof FeatureAssociationTypeElement )
      qname = ((FeatureAssociationTypeElement) parentElement).getAssociationTypeProperty().getQName();
    else
      qname = null;

    if( qname != null )
    {
      final Properties properties = FeatureTypePropertiesCatalog.getProperties( m_workspace.getContext(), qname );
      final String showChildrenString = properties.getProperty( IFeatureTypePropertiesConstants.GMLTREE_SHOW_CHILDREN, IFeatureTypePropertiesConstants.GMLTREE_SHOW_CHILDREN_DEFAULT );
      final boolean showChildren = Boolean.parseBoolean( showChildrenString );
      if( !showChildren )
        return new Object[] {};
    }

    return getChildrenInternal( parentElement );
  }

  private Object[] getChildrenInternal( final Object parentElement )
  {
    if( parentElement instanceof GMLWorkspace )
      return new Object[] { ((GMLWorkspace) parentElement).getRootFeature() };

    final List<Object> result = new ArrayList<Object>();
    if( parentElement instanceof Feature )
      collectFeatureChildren( parentElement, result );
    else if( parentElement instanceof FeatureAssociationTypeElement )
      collectAssociationChildren( (FeatureAssociationTypeElement) parentElement, result );
    else if( parentElement instanceof FeatureList )
      return ((FeatureList) parentElement).toArray(); // can happen in !showAssociation's mode

    return result.toArray();
  }

  private void collectFeatureChildren( final Object parentElement, final List<Object> result )
  {
    final Feature parentFE = (Feature) parentElement;
    final IPropertyType[] properties = parentFE.getFeatureType().getProperties();

    for( final IPropertyType property : properties )
    {
      if( property instanceof IRelationType )
      {
        final FeatureAssociationTypeElement fate = new FeatureAssociationTypeElement( (Feature) parentElement, (IRelationType) property );
        if( m_showAssociations )
          result.add( fate );
        else
          collectAssociationChildren( fate, result );
      }
      else if( GeometryUtilities.isGeometry( property ) )
      {
        final Object value = parentFE.getProperty( property );
        if( value != null )
          result.add( value );
      }
    }
  }

  private void collectAssociationChildren( final FeatureAssociationTypeElement fate, final List<Object> result )
  {
    final Feature parentFeature = (fate).getParentFeature();
    final IRelationType ftp = (fate).getAssociationTypeProperty();
    final Feature[] features = m_workspace.resolveLinks( parentFeature, ftp );
    for( int i = 0; i < features.length; i++ )
    {
      final Feature feature = features[i];
      if( feature != null )
        if( m_workspace.isAggregatedLink( parentFeature, ftp, i ) || feature instanceof XLinkedFeature_Impl )
          result.add( new LinkedFeatureElement2( feature ) );
        else
          result.add( feature );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    /* search is orderd from fast to slow */

    /* If its an association we know the parent */
    if( element instanceof FeatureAssociationTypeElement )
    {
      final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) element;
      return fate.getParentFeature();
    }

    /* Is it one of the root elements? */
    final Object[] elements = getElements( m_workspace );
    for( final Object object : elements )
      if( object == element )
        return null;

    // TODO: there are also GeometryProperty-Elements

    if( element instanceof Feature )
    {
      final Feature feature = (Feature) element;
      final Feature parent = feature.getParent();

      if( !m_showAssociations )
        return parent;

      if( parent != null )
      {
        final Object[] parentChildren = getChildren( parent );
        for( final Object object : parentChildren )
          if( object instanceof FeatureAssociationTypeElement )
          {
            final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) object;
            final IRelationType associationTypeProperty = fate.getAssociationTypeProperty();
            final Object property = parent.getProperty( associationTypeProperty );
            if( property == feature )
              return fate;
            else if( property instanceof List )
              if( ((List< ? >) property).contains( feature ) )
                return fate;
          }
      }
    }

    /* May happen if we have gone-into the tree. */
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
    if( element == null )
      return false;

    final int childCount = getChildren( element ).length;
    return childCount > 0;
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    if( m_workspace == null )
      return new Object[] {};

    final Object[] rootFeatureObjects = new Object[] { m_workspace.getRootFeature() };

    try
    {
      final Object object = findObjectForPath();
      if( object == m_workspace )
        return rootFeatureObjects;

      return getChildren( object );
    }
    catch( final GMLXPathException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );

      return rootFeatureObjects;
    }
  }

  private Object findObjectForPath( ) throws GMLXPathException
  {
    if( m_rootPath.getSegmentSize() == 0 )
      return m_workspace;

    final GMLXPath parentPath = m_rootPath.getParentPath();
    final Object parent = GMLXPathUtilities.query( parentPath, m_workspace );
    if( parent instanceof Feature )
    {
      final GMLXPathSegment segment = m_rootPath.getSegment( m_rootPath.getSegmentSize() - 1 );

      final Object valueFromSegment = GMLXPathUtilities.getValueFromSegment( segment, parent, false );

      final Object[] children = getChildren( parent );
      for( final Object object : children )
        if( object instanceof FeatureAssociationTypeElement )
        {
          final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) object;

          final Object property = ((Feature) parent).getProperty( fate.getAssociationTypeProperty() );
          if( property == valueFromSegment )
            return object;
        }
    }

    return GMLXPathUtilities.query( m_rootPath, m_workspace );
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    if( m_workspace != null )
      m_workspace.removeModellListener( m_workspaceListener );

    m_viewer = null;
    m_workspace = null;
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    m_viewer = (TreeViewer) viewer;

    if( oldInput != newInput )
    {
      if( m_workspace != null )
      {
        m_workspace.removeModellListener( m_workspaceListener );
        m_workspace = null;
      }

      if( newInput instanceof GMLWorkspace )
      {
        m_workspace = (GMLWorkspace) newInput;
        m_workspace.addModellListener( m_workspaceListener );
      }
      else
        m_workspace = null;

// m_rootPath = new GMLXPath( "", null );
    }
  }

  public IRelationType getParentFeatureProperty( final Feature feature )
  {
    // TODO: do not just use the getParent method because root element dont have a parent
    // use a internalParent method instead
    final FeatureAssociationTypeElement parent = (FeatureAssociationTypeElement) getParent( feature );
    if( parent == null )
      return null;

    return parent.getAssociationTypeProperty();
  }

  /** Expand the element and all of its parents */
  public void expandElement( final Object element )
  {
    if( element == null )
      return;

    expandElement( getParent( element ) );

    m_viewer.setExpandedState( element, true );
  }

  public void goInto( final Object object )
  {
    m_viewer.getSelection();
    final Object[] expandedElements = m_viewer.getExpandedElements();

    // prepare for exception
    m_rootPath = new GMLXPath( "", null ); //$NON-NLS-1$

    try
    {
      if( object instanceof Feature )
      {
        final Feature feature = (Feature) object;
        m_rootPath = new GMLXPath( feature );
      }
      else if( object instanceof FeatureAssociationTypeElement )
      {
        final FeatureAssociationTypeElement fate = (FeatureAssociationTypeElement) object;
        final Feature parentFeature = fate.getParentFeature();
        final GMLXPath path = new GMLXPath( parentFeature );
        m_rootPath = new GMLXPath( path, fate.getAssociationTypeProperty().getQName() );
      }
    }
    catch( final GMLXPathException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
    }
    finally
    {
      m_viewer.refresh();
      m_viewer.setExpandedElements( expandedElements );
    }
  }

  public void goUp( )
  {
    final Object[] expandedElements = m_viewer.getExpandedElements();

    try
    {
      final Object object = findObjectForPath();

      final Object parent = getParent( object );
      if( parent != null )
      {
        goInto( parent );
        return; // do not refresh, we already did so
      }
    }
    catch( final GMLXPathException e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoGisPlugin.getDefault().getLog().log( status );
    }

    m_rootPath = new GMLXPath( "", null ); //$NON-NLS-1$

    m_viewer.setExpandedElements( expandedElements );
    m_viewer.refresh();
  }

  public boolean canGoUp( )
  {
    return m_rootPath.getSegmentSize() > 0;
  }

  public GMLXPath getRootPath( )
  {
    return m_rootPath;
  }

  /**
   * Sets the root path and refreshes the tree.
   * <p>
   * Tries to keep the current expansion state.
   * </p>
   */
  public void setRootPath( final GMLXPath rootPath )
  {
    final Object[] expandedElements = m_viewer == null ? null : m_viewer.getExpandedElements();

    try
    {
      m_rootPath = rootPath;
    }
    finally
    {
      if( m_viewer != null )
      {
        m_viewer.setExpandedElements( expandedElements );
        m_viewer.refresh();
      }
    }
  }

  protected void handleModelChanged( final ModellEvent modellEvent )
  {
    if( !m_handleModelEvents )
      return;

    final TreeViewer treeViewer = m_viewer;
    if( treeViewer == null )
      return;

    final Control control = treeViewer.getControl();

    if( modellEvent instanceof FeatureStructureChangeModellEvent )
    {
      final FeatureStructureChangeModellEvent structureEvent = (FeatureStructureChangeModellEvent) modellEvent;
      final Feature[] parentFeature = structureEvent.getParentFeatures();

      if( !control.isDisposed() )
        // REMARK: must be sync, if not we get a racing condition with handleGlobalSelection
        control.getDisplay().syncExec( new Runnable()
        {
          public void run( )
          {
            // This does not work nicely. In order to refresh the tree in a nice way,
            // the modell event should also provide which festures where added/removed
            final Object[] expandedElements = treeViewer.getExpandedElements();

            if( parentFeature == null )
              treeViewer.refresh();
            else
              // for( final Feature feature : parentFeature )
// {
              // treeViewer.refresh( feature ); childs are not updated!
              treeViewer.refresh();
// }

            treeViewer.setExpandedElements( expandedElements );
          }
        } );
    }
    else if( modellEvent instanceof FeaturesChangedModellEvent )
    {
      final FeaturesChangedModellEvent fcme = (FeaturesChangedModellEvent) modellEvent;
      final Feature[] features = fcme.getFeatures();
      if( control != null && !control.isDisposed() )
        control.getDisplay().asyncExec( new Runnable()
        {
          public void run( )
          {
            if( !control.isDisposed() )
              for( final Feature feature : features )
                treeViewer.refresh( feature, true );
          }
        } );
    }
  }

}
