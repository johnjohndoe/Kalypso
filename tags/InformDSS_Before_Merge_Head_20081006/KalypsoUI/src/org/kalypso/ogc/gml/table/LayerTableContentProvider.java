/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.table;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.widgets.Control;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeListener;
import org.kalypso.ogc.gml.KalypsoFeatureThemeSelection;
import org.kalypso.ogc.gml.KalypsoThemeAdapter;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author bce
 */
public class LayerTableContentProvider implements IStructuredContentProvider
{
  private final ISelectionChangedListener m_tableSelectionListener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      viewerSelectionChanged( (IStructuredSelection) event.getSelection() );
    }
  };

  private final IKalypsoThemeListener m_themeListener = new KalypsoThemeAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.KalypsoThemeAdapter#statusChanged(org.kalypso.ogc.gml.IKalypsoTheme)
     */
    @Override
    public void statusChanged( final IKalypsoTheme source )
    {
      handleStatusChanged();
    }
  };

  private final IFeatureSelectionManager m_selectionManager;

  private LayerTableViewer m_viewer;

  public LayerTableContentProvider( final IFeatureSelectionManager selectionManager )
  {
    m_selectionManager = selectionManager;
  }

  /**
   * Input muss ein IKalypsoFeatureTheme sein Output sind die Features
   * 
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final List<Feature> result = new ArrayList<Feature>();
    final FeatureList featureList;
    if( inputElement instanceof IKalypsoFeatureTheme )
      featureList = ((IKalypsoFeatureTheme) inputElement).getFeatureList();
    else if( inputElement instanceof FeatureList )
      featureList = (FeatureList) inputElement;
    else
      return new Object[] {};

    if( featureList == null )
      return new Object[] {};

    final Object[] objects = featureList.toArray();
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) m_viewer.getInput();
    final GMLWorkspace workspace = theme.getWorkspace();
    for( final Object element : objects )
    {
      if( element instanceof Feature )
        result.add( (Feature) element );
      else if( element instanceof String ) // it is a ID
      {
        final Feature feature = workspace.getFeature( (String) element );
        if( feature != null )
          result.add( feature );
      }
    }
    return result.toArray();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose( )
  {
    if( m_viewer != null )
    {
      m_viewer.removeSelectionChangedListener( m_tableSelectionListener );

      final Object input = m_viewer.getInput();
      if( input instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) input;
        (theme).removeKalypsoThemeListener( m_themeListener );
        m_viewer.disposeTheme( theme );
      }
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    if( oldInput instanceof IKalypsoFeatureTheme )
      ((IKalypsoFeatureTheme) oldInput).removeKalypsoThemeListener( m_themeListener );

    if( m_viewer != null )
      m_viewer.removeSelectionChangedListener( m_tableSelectionListener );

    m_viewer = (LayerTableViewer) viewer;

    if( m_viewer != null )
      m_viewer.addSelectionChangedListener( m_tableSelectionListener );

    if( newInput instanceof IKalypsoFeatureTheme )
      ((IKalypsoFeatureTheme) newInput).addKalypsoThemeListener( m_themeListener );
  }

  /**
   * @param selection
   */
  protected void viewerSelectionChanged( final IStructuredSelection selection )
  {
    // remove all features in input from manager
    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) m_viewer.getInput();
    final FeatureList featureList = theme == null ? null : theme.getFeatureList();
    if( featureList == null )
      return;

    // if viewer selection and tree selection are the same, do nothing
    if( m_selectionManager == null )
      return;

    final IStructuredSelection managerSelection = KalypsoFeatureThemeSelection.filter( m_selectionManager.toList(), theme );
    final Object[] managerFeatures = managerSelection.toArray();
    if( Arrays.equalsUnordered( managerFeatures, selection.toArray() ) )
      return;

    final GMLWorkspace workspace = theme.getWorkspace();

    // TODO: remove only previously selected
    // collect elements as "Feature"
    final List<Feature> featureToRemove = new ArrayList<Feature>();
    for( final Iterator iter = featureList.iterator(); iter.hasNext(); )
    {
      final Object element = iter.next();
      if( element instanceof Feature )
        featureToRemove.add( (Feature) element );
      else if( element instanceof String ) // it is the id of a feature
        featureToRemove.add( workspace.getFeature( (String) element ) );
    }

    // add current selection
    final List<EasyFeatureWrapper> wrappers = new ArrayList<EasyFeatureWrapper>( selection.size() );
    for( final Iterator sIt = selection.iterator(); sIt.hasNext(); )
    {
      final Object object = sIt.next();
      if( object instanceof Feature )
      {
        final EasyFeatureWrapper wrapper = new EasyFeatureWrapper( theme.getWorkspace(), (Feature) object, featureList.getParentFeature(), featureList.getParentFeatureTypeProperty() );
        wrappers.add( wrapper );
      }
    }

    final EasyFeatureWrapper[] izis = wrappers.toArray( new EasyFeatureWrapper[wrappers.size()] );
    final Feature[] featureArray = featureToRemove.toArray( new Feature[featureToRemove.size()] );
    m_selectionManager.changeSelection( featureArray, izis );
  }

  protected void handleStatusChanged( )
  {
    final LayerTableViewer viewer = m_viewer;
    if( viewer != null && !viewer.isDisposed() )
    {
      final Control control = viewer.getControl();
      control.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          if( !control.isDisposed() )
            viewer.refreshAll();
        }
      } );
    }
  }

}