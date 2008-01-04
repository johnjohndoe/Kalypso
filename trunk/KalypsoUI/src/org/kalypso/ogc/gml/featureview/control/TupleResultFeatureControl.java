/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.swt.custom.ExcelTableCursor;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.ChangeFeatureCommand;
import org.kalypso.ogc.gml.om.FeatureComponent;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.om.table.LastLineCellModifier;
import org.kalypso.ogc.gml.om.table.LastLineContentProvider;
import org.kalypso.ogc.gml.om.table.LastLineLabelProvider;
import org.kalypso.ogc.gml.om.table.TupleResultCellModifier;
import org.kalypso.ogc.gml.om.table.TupleResultContentProvider;
import org.kalypso.ogc.gml.om.table.TupleResultLabelProvider;
import org.kalypso.ogc.gml.om.table.handlers.ComponentUiHandlerFactory;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;
import org.kalypso.template.featureview.ColumnDescriptor;
import org.kalypso.util.swt.SWTUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class TupleResultFeatureControl extends AbstractFeatureControl implements ITupleResultChangedListener
{
  private final List<ModifyListener> m_listener = new ArrayList<ModifyListener>( 10 );

  private final IComponentUiHandler[] m_handlers;

  private DefaultTableViewer m_viewer;

  private ViewerFilter m_viewerFilter;

  private TupleResultContentProvider m_tupleResultContentProvider;

  private LastLineContentProvider m_lastLineContentProvider;

  private TupleResultLabelProvider m_tupleResultLabelProvider;

  private LastLineLabelProvider m_lastLineLabelProvider;

  private Color m_lastLineBackground;

  private TupleResult m_tupleResult;

  /** TRICK: in order to suppress refresh after our own changes we set this flag. */
  private int m_ignoreNextUpdateControl = 0;

  public TupleResultFeatureControl( final Feature feature, final IPropertyType ftp, final IComponentUiHandler[] handlers )
  {
    super( feature, ftp );

    m_handlers = handlers;
  }

  public static IComponentUiHandler[] toHandlers( final Feature feature, final ColumnDescriptor[] descriptors )
  {
    final List<IComponentUiHandler> handlers = new ArrayList<IComponentUiHandler>( descriptors.length );

    if( feature == null )
      return new IComponentUiHandler[0];

    /* result definition */
    final Feature resultDefinition = (Feature) feature.getProperty( new QName( NS.OM, "resultDefinition" ) );
    final GMLWorkspace workspace = resultDefinition.getWorkspace();

    /* Directly read components from feature, without converting the whole stuff into an observation */
    final List< ? > components = (List< ? >) resultDefinition.getProperty( new QName( NS.SWE, "component" ) );
    final Map<String, IComponent> componentMap = new HashMap<String, IComponent>();
    for( final Object object : components )
    {
      final Feature componentFeature = FeatureHelper.getFeature( workspace, object );
      final IComponent component = new FeatureComponent( componentFeature );
      componentMap.put( component.getId(), component );
    }

    for( final ColumnDescriptor cd : descriptors )
    {
      final String componentId = cd.getComponent();
      final IComponent component = componentMap.get( componentId );
      final int alignment = SWTUtilities.createStyleFromString( cd.getAlignment() );

      if( component != null )
      {
        final IComponentUiHandler handler = ComponentUiHandlerFactory.getHandler( component, cd.isEditable(), cd.isResizeable(), cd.getLabel(), alignment, cd.getWidth(), cd.getWidthPercent(), cd.getDisplayFormat(), cd.getNullFormat(), cd.getParseFormat() );
        handlers.add( handler );
      }

      final boolean optional = cd.isOptional();
      if( component == null && !optional )
      {
        /* Non-optional columns must exists: throw error message */
        final String msg = String.format( "Non-Optional component does not exist: %s. Remove from template or make optional.", componentId );
        throw new IllegalArgumentException( msg );
      }
    }

    return handlers.toArray( new IComponentUiHandler[handlers.size()] );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#createControl(org.eclipse.swt.widgets.Composite, int)
   */
  public Control createControl( final Composite parent, final int style )
  {
    m_viewer = new DefaultTableViewer( parent, style );
    final Table table = m_viewer.getTable();
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    m_lastLineBackground = new Color( parent.getDisplay(), 170, 230, 255 );

    m_tupleResultContentProvider = new TupleResultContentProvider( m_handlers );
    m_lastLineContentProvider = new LastLineContentProvider( m_tupleResultContentProvider );
    m_tupleResultLabelProvider = new TupleResultLabelProvider( m_tupleResultContentProvider.getHandlers() );
    m_lastLineLabelProvider = new LastLineLabelProvider( m_tupleResultLabelProvider, m_lastLineBackground );

    if( m_viewerFilter != null )
      m_viewer.addFilter( m_viewerFilter );

    final TupleResultCellModifier tupleResultCellModifier = new TupleResultCellModifier( m_tupleResultContentProvider );

    final TupleResultContentProvider tupleResultContentProvider = m_tupleResultContentProvider;
    final LastLineCellModifier lastLineCellModifier = new LastLineCellModifier( tupleResultCellModifier )
    {
      @Override
      protected Object createNewElement( )
      {
        final TupleResult result = tupleResultContentProvider.getResult();
        if( result != null )
          return result.createRecord();

        return null;
      }

      @Override
      protected void addElement( final Object newElement, final String property, final Object value )
      {
        final TupleResult result = tupleResultContentProvider.getResult();

        final IRecord record = (IRecord) newElement;
        tupleResultCellModifier.modifyRecord( record, property, value );

        result.add( record );
      }
    };

    m_viewer.setContentProvider( m_lastLineContentProvider );
    m_viewer.setLabelProvider( m_lastLineLabelProvider );
    m_viewer.setCellModifier( lastLineCellModifier );
    m_viewer.setInput( null );

    updateControl();

    new ExcelTableCursor( m_viewer, SWT.NONE, ExcelTableCursor.ADVANCE_MODE.RIGHT, true );

    return table;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.AbstractFeatureControl#dispose()
   */
  @Override
  public void dispose( )
  {
    m_tupleResultContentProvider.dispose();
    m_tupleResultLabelProvider.dispose();

    m_lastLineContentProvider.dispose();
    m_lastLineLabelProvider.dispose();

    m_lastLineBackground.dispose();

    if( m_tupleResult != null )
    {
      m_tupleResult.removeChangeListener( this );
      m_tupleResult = null;
    }

    super.dispose();
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#updateControl()
   */
  public void updateControl( )
  {
    if( m_ignoreNextUpdateControl > 0 )
    {
      m_ignoreNextUpdateControl--;
      return;
    }

    final Feature feature = getObservationFeature();
    if( m_tupleResult != null )
      m_tupleResult.removeChangeListener( this );

    final IObservation<TupleResult> obs = feature == null ? null : ObservationFeatureFactory.toObservation( feature );
    m_tupleResult = obs == null ? null : obs.getResult();

    if( m_tupleResult != null )
      m_tupleResult.addChangeListener( this );

    m_viewer.setInput( m_tupleResult );
  }

  /**
   * Returns the observation.
   * <p>
   * If the given property is a relation type, get the feature from that property, else directly use the given feature
   * of this control.
   * </p>
   */
  private Feature getObservationFeature( )
  {
    final Feature feature = getFeature();
    final IPropertyType ftp = getFeatureTypeProperty();

    return getObservationFeature( feature, ftp );
  }

  static Feature getObservationFeature( final Feature feature, final IPropertyType ftp )
  {
    if( ftp instanceof IRelationType )
    {
      final Object property = feature.getProperty( ftp );
      return FeatureHelper.getFeature( feature.getWorkspace(), property );
    }

    return feature;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#isValid()
   */
  public boolean isValid( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#addModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void addModifyListener( final ModifyListener l )
  {
    m_listener.add( l );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.control.IFeatureControl#removeModifyListener(org.eclipse.swt.events.ModifyListener)
   */
  public void removeModifyListener( final ModifyListener l )
  {
    m_listener.remove( l );
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#valuesChanged(org.kalypso.observation.result.ITupleResultChangedListener.ValueChange[])
   */
  public void valuesChanged( final ValueChange[] changes )
  {
    fireChanges( false );
    // fireModified();
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#recordsChanged(org.kalypso.observation.result.IRecord[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void recordsChanged( final IRecord[] records, final TYPE type )
  {
    fireChanges( false );
    // fireModified();
  }

  /**
   * @see org.kalypso.observation.result.ITupleResultChangedListener#componentsChanged(org.kalypso.observation.result.IComponent[],
   *      org.kalypso.observation.result.ITupleResultChangedListener.TYPE)
   */
  public void componentsChanged( final IComponent[] components, final TYPE type )
  {
    fireChanges( true );
  }

  private void fireChanges( final boolean definitionChanged )
  {
    final Feature obsFeature = getObservationFeature();
    final IFeatureType obsFT = obsFeature.getFeatureType();
    final IRelationType resultDefPT = (IRelationType) obsFT.getProperty( ObservationFeatureFactory.OM_RESULTDEFINITION );
    final IPropertyType resultPT = obsFT.getProperty( ObservationFeatureFactory.OM_RESULT );

    final Feature rd = ObservationFeatureFactory.buildRecordDefinition( obsFeature, resultDefPT, m_tupleResult.getComponents(), m_tupleResult.getSortComponents() );

    final String strResult = ObservationFeatureFactory.serializeResultAsString( m_tupleResult );

    // PROBLEM: we have 2 changes, so we get entries to the undo queue here
    // TODO: refaktor so that we may send multiple changes at one go
    if( definitionChanged )
    {
      m_ignoreNextUpdateControl++;
      fireFeatureChange( new ChangeFeatureCommand( obsFeature, resultDefPT, rd ) );
    }

    m_ignoreNextUpdateControl++;
    fireFeatureChange( new ChangeFeatureCommand( obsFeature, resultPT, strResult ) );
  }

  /**
   * must be called before createControl() is called!
   */
  public void setViewerFilter( final ViewerFilter filter )
  {
    m_viewerFilter = filter;
  }
}
