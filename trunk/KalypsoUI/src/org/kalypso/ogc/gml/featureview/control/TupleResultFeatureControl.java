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
package org.kalypso.ogc.gml.featureview.control;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.contribs.eclipse.swt.custom.ExcelTableCursor;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.ITupleResultChangedListener;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ogc.gml.om.table.LastLineCellModifier;
import org.kalypso.ogc.gml.om.table.LastLineContentProvider;
import org.kalypso.ogc.gml.om.table.LastLineLabelProvider;
import org.kalypso.ogc.gml.om.table.TupleResultCellModifier;
import org.kalypso.ogc.gml.om.table.TupleResultContentProvider;
import org.kalypso.ogc.gml.om.table.TupleResultLabelProvider;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class TupleResultFeatureControl extends AbstractFeatureControl implements ITupleResultChangedListener
{
  private final List<ModifyListener> m_listener = new ArrayList<ModifyListener>( 10 );

  private DefaultTableViewer m_viewer;

  private TupleResultContentProvider m_tupleResultContentProvider;

  private LastLineContentProvider m_lastLineContentProvider;

  private TupleResultLabelProvider m_tupleResultLabelProvider;

  private LastLineLabelProvider m_lastLineLabelProvider;

  private Color m_lastLineBackground;

  private TupleResult m_tupleResult;

  /** TRICK: in order to supress refresh after our own changes we set this flag. */
  private boolean m_ignoreNextUpdateControl = false;

  public TupleResultFeatureControl( final IPropertyType ftp )
  {
    super( ftp );
  }

  public TupleResultFeatureControl( final Feature feature, final IPropertyType ftp )
  {
    super( feature, ftp );
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

    m_tupleResultContentProvider = new TupleResultContentProvider();
    m_lastLineContentProvider = new LastLineContentProvider( m_tupleResultContentProvider );
    m_tupleResultLabelProvider = new TupleResultLabelProvider();
    m_lastLineLabelProvider = new LastLineLabelProvider( m_tupleResultLabelProvider, m_lastLineBackground );

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
    if( m_ignoreNextUpdateControl )
    {
      m_ignoreNextUpdateControl = false;
      return;
    }

    final Feature feature = getFeature();

    if( m_tupleResult != null )
      m_tupleResult.removeChangeListener( this );

    final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( feature );
    m_tupleResult = obs == null ? null : obs.getResult();

    if( m_tupleResult != null )
      m_tupleResult.addChangeListener( this );

    m_viewer.setInput( m_tupleResult );
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
  public void recordsChanged( final IRecord[] records, TYPE type )
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
    // fireModified();
  }

  /** Should we forward events from the cell-editors here? */
  // private void fireModified( )
  // {
  // final Event event = new Event();
  // event.display = m_viewer.getTable().getDisplay();
  // event.item = m_viewer.getTable();
  //
  // final ModifyEvent modifyEvent = new ModifyEvent( event );
  //
  // for( final ModifyListener l : m_listener )
  // {
  // try
  // {
  // l.modifyText( modifyEvent );
  // }
  // catch( final Throwable e )
  // {
  // final IStatus status = StatusUtilities.statusFromThrowable( e );
  // KalypsoGisPlugin.getDefault().getLog().log( status );
  // }
  // }
  // }
  
  
  private void fireChanges( final boolean definitionChanged )
  {
    final Feature obsFeature = getFeature();

    final Feature rd = ObservationFeatureFactory.buildRecordDefinition( obsFeature, m_tupleResult.getComponents() );

    final IFeatureType obsFT = obsFeature.getFeatureType();

    final IPropertyType resultDefPT = obsFT.getProperty( ObservationFeatureFactory.OM_RESULTDEFINITION );
    final IPropertyType resultPT = obsFT.getProperty( ObservationFeatureFactory.OM_RESULT );

    final String strResult = ObservationFeatureFactory.serializeResultAsString( m_tupleResult );

    // PROBLEM: we have 2 changes, so we get entries to the undo queue here
    // TODO: refaktor so that we may send multiple changes at one go
    if( definitionChanged )
    {
      final FeatureChange change = new FeatureChange( obsFeature, resultDefPT, rd );
      m_ignoreNextUpdateControl = true;
      fireFeatureChange( change );
    }

    final FeatureChange change2 = new FeatureChange( obsFeature, resultPT, strResult );
    m_ignoreNextUpdateControl = true;
    fireFeatureChange( change2 );
  }

}
