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
package org.kalypso.ui.rrm.wizards;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.ogc.gml.serialize.ShapeSerializer;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.gml.schema.SpecialPropertyMapper;

/**
 * @author Gernot Belger
 */
public class SourceMappingComposite extends Composite
{
  public class VPTLabelProvider extends LabelProvider
  {
    /**
     * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
     */
    @Override
    public String getText( final Object element )
    {
      if( element instanceof IValuePropertyType )
      {
        final IValuePropertyType sourcePT = (IValuePropertyType) element;
        return sourcePT.getQName().getLocalPart();
      }

      return super.getText( element );
    }
  }

  public class ValidPropertyMappingFilter extends ViewerFilter
  {
    private final IValuePropertyType m_targetPT;

    public ValidPropertyMappingFilter( final IValuePropertyType targetPT )
    {
      m_targetPT = targetPT;
    }

    /**
     * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.Viewer, java.lang.Object,
     *      java.lang.Object)
     */
    @Override
    public boolean select( final Viewer viewer, final Object parentElement, final Object element )
    {
      if( element instanceof IValuePropertyType )
      {
        final IValuePropertyType sourcePT = (IValuePropertyType) element;
        return SpecialPropertyMapper.isValidMapping( sourcePT.getValueClass(), m_targetPT.getValueClass() );
      }

      return true;
    }
  }

  private static final String EMPTY_KEY = Messages.getString("SourceMappingComposite_0"); //$NON-NLS-1$

  private static final String NULL_KEY = "-NULL-"; //$NON-NLS-1$

  private final Map<ComboViewer, IValuePropertyType> m_combos = new HashMap<ComboViewer, IValuePropertyType>();

  private final Map<IValuePropertyType, IValuePropertyType> m_mapping = new HashMap<IValuePropertyType, IValuePropertyType>();

  private FeatureList m_sourceData;

  public SourceMappingComposite( final Composite parent, final int style, final IValuePropertyType[] targetProperties )
  {
    super( parent, style );

    setLayout( new GridLayout( 2, false ) );

    for( final IValuePropertyType targetPT : targetProperties )
      addTargetProperty( targetPT );

    updateSourceList();
  }

  private void addTargetProperty( final IValuePropertyType targetPT )
  {
    final IAnnotation annotation = targetPT.getAnnotation();

    final Label text = new Label( this, SWT.NONE );
    text.setLayoutData( new GridData( SWT.LEFT, SWT.CENTER, false, false ) );
    text.setText( annotation.getLabel() );
    text.setToolTipText( annotation.getTooltip() );

    final ComboViewer comboViewer = new ComboViewer( this, SWT.READ_ONLY | SWT.DROP_DOWN );
    comboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    comboViewer.setLabelProvider( new VPTLabelProvider() );
    comboViewer.setContentProvider( new ArrayContentProvider() );

    m_combos.put( comboViewer, targetPT );

    comboViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        final IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        handleComboSelectionChanged( targetPT, selection );
      }
    } );
  }

  protected void handleComboSelectionChanged( final IValuePropertyType targetPT, final IStructuredSelection selection )
  {
    if( selection.isEmpty() )
      m_mapping.remove( targetPT );
    else
    {
      final Object firstElement = selection.getFirstElement();
      if( firstElement instanceof IValuePropertyType )
        m_mapping.put( targetPT, (IValuePropertyType) firstElement );
      else
        m_mapping.remove( firstElement );
    }
  }

  public void resetSelection( )
  {
    for( final ComboViewer viewer : m_combos.keySet() )
      resetSelection( viewer );
  }

  private void resetSelection( final ComboViewer comboViewer )
  {
    final Object element = findBestSelection( comboViewer );
    comboViewer.setSelection( new StructuredSelection( element ) );
  }

  private Object findBestSelection( final ComboViewer comboViewer )
  {
    final IValuePropertyType targetPT = m_combos.get( comboViewer );
    final Object input = comboViewer.getInput();
    final Object[] elements = ((IStructuredContentProvider) comboViewer.getContentProvider()).getElements( input );

    if( targetPT.isGeometry() )
      return elements[elements.length - 1];

    return elements[0];
  }

  /**
   * This method clears the mapping
   */
  private void updateSourceList( )
  {
    final IValuePropertyType[] ftp = getSourceProperties();

    final Object[] input;
    if( ftp == null )
      input = new Object[] { EMPTY_KEY };
    else
      input = ArrayUtils.addAll( new Object[] { NULL_KEY }, ftp );

    final Set<Entry<ComboViewer, IValuePropertyType>> entrySet = m_combos.entrySet();
    for( final Entry<ComboViewer, IValuePropertyType> entry : entrySet )
    {
      final ComboViewer comboViewer = entry.getKey();
      final IValuePropertyType targetPT = entry.getValue();

      if( ftp == null )
        comboViewer.setFilters( new ViewerFilter[0] );
      else
      {
        final ViewerFilter filter = new ValidPropertyMappingFilter( targetPT );
        comboViewer.setFilters( new ViewerFilter[] { filter } );
      }

      comboViewer.setInput( input );

      /* Select first element */
      resetSelection( comboViewer );
    }
  }

  private IValuePropertyType[] getSourceProperties( )
  {
    if( m_sourceData == null )
      return null;

    final List<IValuePropertyType> result = new ArrayList<IValuePropertyType>();

    final IFeatureType srcFT = m_sourceData.getParentFeatureTypeProperty().getTargetFeatureType();
    final IPropertyType[] ftp = srcFT.getProperties();

    for( final IPropertyType element : ftp )
    {
      if( element instanceof IValuePropertyType )
      {
        final IValuePropertyType fromPT = (IValuePropertyType) element;
        result.add( fromPT );
      }
    }

    return result.toArray( new IValuePropertyType[result.size()] );
  }

  public Map<IValuePropertyType, IValuePropertyType> getMapping( )
  {
    return m_mapping;
  }

  public void setSourceWorkspace( final GMLWorkspace shapeWorkspace )
  {
    final Feature rootFeature = shapeWorkspace.getRootFeature();
    m_sourceData = (FeatureList) rootFeature.getProperty( ShapeSerializer.PROPERTY_FEATURE_MEMBER );

    updateSourceList();
  }

  public List< ? > getSourceData( )
  {
    return m_sourceData;
  }

}
