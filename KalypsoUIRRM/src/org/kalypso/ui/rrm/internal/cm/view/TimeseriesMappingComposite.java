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
package org.kalypso.ui.rrm.internal.cm.view;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.ViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.contribs.eclipse.jface.viewers.ColumnViewerUtil;
import org.kalypso.contribs.eclipse.jface.viewers.ViewerColumnItem;
import org.kalypso.contribs.eclipse.jface.viewers.table.ColumnsResizeControlListener;
import org.kalypso.contribs.eclipse.swt.widgets.ColumnViewerSorter;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.model.hydrology.binding.timeseriesMappings.IMappingElement;
import org.kalypso.model.hydrology.binding.timeseriesMappings.ITimeseriesMapping;
import org.kalypso.model.hydrology.binding.timeseriesMappings.TimeseriesMappingType;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class TimeseriesMappingComposite extends FeatureBeanComposite<ITimeseriesMapping>
{
  public TimeseriesMappingComposite( final Composite parent, final IDataBinding binding, final FeatureBean<ITimeseriesMapping> bean, final boolean generalEditable )
  {
    super( parent, bean, binding, generalEditable );
  }

  @Override
  protected void createContents( )
  {
    createPropertyTextFieldControl( Feature.QN_DESCRIPTION );
    createPropertyTextFieldControl( ITimeseriesMapping.PROPERTY_COMMENT );

    if( isEditable() )
    {
      final Label label = createPropertyLabel( this, ITimeseriesMapping.MEMBER_MAPPING );

      final TimeseriesMappingBean bean = (TimeseriesMappingBean) getBean();
      final TimeseriesMappingType mappingType = bean.getMappingType();

      label.setText( String.format( Messages.getString( "TimeseriesMappingComposite_0" ), mappingType.getLabel() ) ); //$NON-NLS-1$

      createMappingTable();
    }
    else
      createPropertyDateTimeControl( ITimeseriesMapping.PROPERTY_LAST_MODIFIED );
  }

  private void createMappingTable( )
  {
    final TimeseriesMappingBean bean = (TimeseriesMappingBean) getBean();

    final Table table = new Table( this, SWT.FULL_SELECTION | SWT.SINGLE | SWT.BORDER );
    table.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    table.setHeaderVisible( true );

    final TableViewer tableViewer = new TableViewer( table );
    tableViewer.setContentProvider( new ArrayContentProvider() );

    /* Name column */
    final ViewerColumn nameColumn = ColumnViewerUtil.createViewerColumn( tableViewer, SWT.LEFT );
    nameColumn.setLabelProvider( new MappingElementBeanNameLabelProvider() );

    final ViewerColumnItem nameItem = new ViewerColumnItem( nameColumn );
    setPropertyLabel( nameItem, IMappingElement.QN_NAME );
    nameItem.setMoveable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( nameItem.getColumn() );
    ColumnViewerSorter.registerSorter( nameColumn, new MappingElementBeanNameViewerComparator() );

    /* Description column */
    final ViewerColumn descriptionColumn = ColumnViewerUtil.createViewerColumn( tableViewer, SWT.LEFT );
    descriptionColumn.setLabelProvider( new MappingElementBeanDescriptionLabelProvider() );

    final ViewerColumnItem descriptionItem = new ViewerColumnItem( descriptionColumn );
    setPropertyLabel( descriptionItem, IMappingElement.QN_DESCRIPTION );
    descriptionItem.setMoveable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( descriptionItem.getColumn() );
    ColumnViewerSorter.registerSorter( descriptionColumn, new MappingElementBeanDescriptionViewerComparator() );

    /* Link column */
    final ViewerColumn linkColumn = ColumnViewerUtil.createViewerColumn( tableViewer, SWT.LEFT );
    linkColumn.setLabelProvider( new MappingElementBeanLinkLabelProvider() );

    final ViewerColumnItem linkItem = new ViewerColumnItem( linkColumn );
    setPropertyLabel( linkItem, IMappingElement.PROPERTY_TIMESERIES_LINK );
    linkItem.setMoveable( false );
    ColumnsResizeControlListener.setMinimumPackWidth( linkItem.getColumn() );
    ColumnViewerSorter.registerSorter( linkColumn, new MappingElementBeanLinkViewerComparator() );

    linkColumn.setEditingSupport( new MappingElementBeanLinkEditingSupport( tableViewer, bean.getMappingType() ) );

    /* content */
    tableViewer.setInput( bean.getMappings() );

    table.addControlListener( new ColumnsResizeControlListener() );

    ColumnViewerSorter.setSortState( nameColumn, Boolean.FALSE );
  }

  private void setPropertyLabel( final ViewerColumnItem nameItem, final QName propertyName )
  {
    final IFeatureType meType = GMLSchemaUtilities.getFeatureTypeQuiet( IMappingElement.FEATURE_MAPPING_ELEMENT );

    final IPropertyType property = meType.getProperty( propertyName );
    final IAnnotation annotation = property.getAnnotation();
    nameItem.setText( annotation.getLabel() );
    nameItem.setToolTipText( annotation.getTooltip() );
  }
}