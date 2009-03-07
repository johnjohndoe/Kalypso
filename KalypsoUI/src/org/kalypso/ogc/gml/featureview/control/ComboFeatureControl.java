/*--------------- Kalypso-Header ------------------------------------------

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

 --------------------------------------------------------------------------*/

package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.PropertyUtils;
import org.kalypso.gmlschema.property.relation.IDocumentReference;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ui.editor.gmleditor.ui.GMLLabelProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.visitors.CollectorVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.FeatureSubstitutionVisitor;

/**
 * This feature control is a combo box, which just sets the feature-value to the given value when selected.
 * <p>
 * Today only properties with String type are supported.
 * </p>
 *
 * @author Gernot Belger
 */
public class ComboFeatureControl extends AbstractFeatureControl
{
  private static final Object NULL_LINK = new Object();

  private final ISelectionChangedListener m_listener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      comboSelected( (IStructuredSelection) event.getSelection() );
    }
  };

  private final List<ModifyListener> m_listeners = new ArrayList<ModifyListener>( 5 );

  private ComboViewer m_comboViewer = null;

  private final Map<Object, String> m_fixedEntries = new LinkedHashMap<Object, String>();

  private final LinkedHashMap<Object, String> m_entries = new LinkedHashMap<Object, String>();

  private boolean m_ignoreNextUpdate = false;

  /**
   * Used for sorting the elements in the combobox.
   */
  private final ViewerComparator m_comparator;

  public ComboFeatureControl( final IPropertyType ftp, final Map<Object, String> entries, final ViewerComparator comparator )
  {
    this( null, ftp, entries, comparator );
  }

  public ComboFeatureControl( final Feature feature, final IPropertyType ftp, final Map<Object, String> entries, final ViewerComparator comparator )
  {
    super( feature, ftp );

    if( entries != null )
      m_fixedEntries.putAll( entries );

    m_comparator = comparator;
  }

  private void updateEntries( final IPropertyType ftp )
  {
    m_entries.clear();

    if( ftp instanceof IValuePropertyType )
    {
      final IValuePropertyType vpt = (IValuePropertyType) ftp;
      final Map<Object, String> createComboEntries = PropertyUtils.createComboEntries( vpt );
      m_entries.putAll( createComboEntries );

      if( vpt.isFixed() )
        m_comboViewer.getControl().setEnabled( false );

      return;
    }

    if( ftp instanceof IRelationType )
    {
      final IRelationType rt = (IRelationType) ftp;
      if( !rt.isInlineAble() && rt.isLinkAble() )
      {
        /* Null entry to delete link if this is allowed */
        if( rt.isNillable() )
          m_entries.put( NULL_LINK, "<kein Link>" ); //$NON-NLS-1$

        /* Find all substituting features. */
        final Feature feature = getFeature();

        final GMLWorkspace workspace = feature.getWorkspace();

        final Feature[] features = collectReferencableFeatures( workspace, feature, rt );

        final GMLLabelProvider labelProvider = new GMLLabelProvider();

        for( final Feature foundFeature : features )
        {
          if( foundFeature instanceof XLinkedFeature_Impl )
            m_entries.put( foundFeature, labelProvider.getText( foundFeature ) );
          else
            m_entries.put( foundFeature.getId(), labelProvider.getText( foundFeature ) );
        }
      }
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_comboViewer != null && !m_comboViewer.getControl().isDisposed() )
      m_comboViewer.removeSelectionChangedListener( m_listener );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_comboViewer = new ComboViewer( parent, style );

    m_comboViewer.setContentProvider( new ArrayContentProvider() );

    final Map<Object, String> entries = m_entries;
    m_comboViewer.setLabelProvider( new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( entries.containsKey( element ) )
          return entries.get( element );

        return super.getText( element );
      }
    } );

    /* Set the comparator, if any was given. */
    if( m_comparator != null )
      m_comboViewer.setComparator( m_comparator );

    m_comboViewer.setInput( m_entries.keySet() );

    m_comboViewer.addSelectionChangedListener( m_listener );

    updateControl();

    return m_comboViewer.getControl();
  }

  protected void comboSelected( final IStructuredSelection selection )
  {
    final Feature feature = getFeature();
    final IPropertyType pt = getFeatureTypeProperty();

    final Object oldValue = getCurrentFeatureValue();
    final Object newSelection = selection.isEmpty() ? null : selection.getFirstElement();
    final Object newValue = newSelection == NULL_LINK ? null : newSelection;

    /* Null check first */
    if( newValue == oldValue )
      return;

    if( (newValue == null && oldValue != null) || !newValue.equals( oldValue ) )
    {
      m_ignoreNextUpdate = true;
      fireFeatureChange( new ChangeFeatureCommand( feature, pt, newValue ) );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    if( m_ignoreNextUpdate )
    {
      m_ignoreNextUpdate = false;
      return;
    }

    final Object currentFeatureValue = getCurrentFeatureValue();

    updateEntries( getFeatureTypeProperty() );

    m_comboViewer.refresh();

    for( final Object value : m_entries.keySet() )
    {// TODO
      if( value.equals( currentFeatureValue ) || (value == NULL_LINK && currentFeatureValue == null) )
      {
        m_comboViewer.setSelection( new StructuredSelection( value ), true );
        break;
      }
    }
  }

  /** Returns the current value of the feature as string. */
  private Object getCurrentFeatureValue( )
  {
    return getFeature().getProperty( getFeatureTypeProperty() );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    // a radio button is always valid
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

  public static Feature[] collectReferencableFeatures( final GMLWorkspace localWorkspace, final Feature parentFeature, final IRelationType rt )
  {
    final IFeatureType targetFeatureType = rt.getTargetFeatureType();

    final List<Feature> foundFeatures = new ArrayList<Feature>();

    final IDocumentReference[] refs = rt.getDocumentReferences();
    for( final IDocumentReference ref : refs )
      collectFromDocument( localWorkspace, parentFeature, rt, targetFeatureType, foundFeatures, ref );

    /* Special case: if we already have a link, we may guess the referenced document */
    final Object value = parentFeature.getProperty( rt );
    if( value instanceof XLinkedFeature_Impl )
    {
      final XLinkedFeature_Impl xlink = (XLinkedFeature_Impl) value;
      final String href = xlink.getHref();
      final int indexOfHref = href.indexOf( '#' );
      final String uri = indexOfHref == -1 ? null : href.substring( 0, indexOfHref );
      final Feature linkedFeature = xlink.getFeature();
      if( linkedFeature != null && uri != null )
        collectFromWorkspace( localWorkspace, parentFeature, rt, targetFeatureType, foundFeatures, uri, linkedFeature.getWorkspace() );
    }

    return foundFeatures.toArray( new Feature[foundFeatures.size()] );
  }

  private static void collectFromDocument( final GMLWorkspace localWorkspace, final Feature parentFeature, final IRelationType rt, final IFeatureType targetFeatureType, final List<Feature> foundFeatures, final IDocumentReference ref )
  {
    final String uri = ref.getReference();
    final GMLWorkspace workspace;
    if( ref == IDocumentReference.SELF_REFERENCE )
      workspace = localWorkspace;
    else
      workspace = localWorkspace.getLinkedWorkspace( uri );

    collectFromWorkspace( localWorkspace, parentFeature, rt, targetFeatureType, foundFeatures, uri, workspace );
  }

  private static void collectFromWorkspace( final GMLWorkspace localWorkspace, final Feature parentFeature, final IRelationType rt, final IFeatureType targetFeatureType, final List<Feature> foundFeatures, final String uri, final GMLWorkspace workspace )
  {
    if( workspace == null )
      return;

    final CollectorVisitor collectorVisitor = new CollectorVisitor();
    final FeatureVisitor fv = new FeatureSubstitutionVisitor( collectorVisitor, targetFeatureType );

    workspace.accept( fv, workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

    final Feature[] features = collectorVisitor.getResults( true );
    for( final Feature feature : features )
    {
      if( workspace == localWorkspace )
        foundFeatures.add( feature );
      else
      {
        final String href = uri + "#" + feature.getId(); //$NON-NLS-1$
        final XLinkedFeature_Impl linkedFeature = new XLinkedFeature_Impl( parentFeature, rt, targetFeatureType, href, "", "", "", "", "" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        foundFeatures.add( linkedFeature );
      }
    }
  }
}
