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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.io.IOException;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.databinding.wizard.WizardPageSupport;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.validation.ObjectNotSameValidator;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.db.constants.WaterBodyConstants.STATIONING_DIRECTION;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBFFieldLabelProvider;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.IDBFField;

/**
 * @author Gernot Belger
 */
public class ImportWaterbodiesSelectAttributesPage extends WizardPage implements IUpdateable
{
  private final ImportWaterBodiesData m_data;

  private StatusComposite m_geometryStatusComposite;

  private DataBindingContext m_binding;

  protected ImportWaterbodiesSelectAttributesPage( final String pageName, final ImportWaterBodiesData data )
  {
    super( pageName );

    setTitle( "Map Attributes" );
    setDescription( "Map the attributes of the shape files to attributes of water bodies." );

    m_data = data;
  }

  @Override
  public void createControl( final Composite parent )
  {
    m_binding = new DataBindingContext();

    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    createGeometryCheckControl( panel );

    final Group attributeGroup = new Group( panel, SWT.NONE );
    attributeGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    attributeGroup.setText( "Attributes" );
    GridLayoutFactory.swtDefaults().numColumns( 3 ).equalWidth( false ).applyTo( attributeGroup );

    createCodeControl( attributeGroup );
    createNameControl( attributeGroup );
    createDescriptionControl( attributeGroup );
    createDirectionControl( attributeGroup );

    WizardPageSupport.create( this, m_binding );

    setErrorMessage( null );
  }

  private void createCodeControl( final Composite parent )
  {
    createAttributeControl( "River Code", WaterBody.PROPERTY_NAME, parent, false );

    /* No default control: code must be taken from shape */
    new Label( parent, SWT.NONE );
  }

  private void createNameControl( final Composite parent )
  {
    createAttributeControl( "Name", WaterBody.PROPERTY_LABEL, parent, false );

    /* No default control: code must be taken from shape */
    new Label( parent, SWT.NONE );
  }

  private void createDescriptionControl( final Composite parent )
  {
    final ImportAttributeInfo info = createAttributeControl( "Description", WaterBody.PROPERTY_DESCRIPTION, parent, true );

    final Text text = new Text( parent, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setMessage( "Description for all water bodies" );

    final ISWTObservableValue targetValue = SWTObservables.observeText( text, SWT.Modify );
    final ISWTObservableValue targetEnablement = SWTObservables.observeEnabled( text );

    final IObservableValue modelValue = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_DEFAULT_VALUE );
    final IObservableValue modelEnablement = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_ENABLEMENT );

    m_binding.bindValue( targetValue, modelValue );
    m_binding.bindValue( targetEnablement, modelEnablement );
  }

  private void createDirectionControl( final Composite parent )
  {
    final ImportAttributeInfo info = createAttributeControl( "Directection of Stationing", WaterBody.PROPERTY_DIRECTION_OF_STATIONING, parent, true );

    info.setDefaultValue( STATIONING_DIRECTION.upstream );

    final ComboViewer directionViewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    directionViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    directionViewer.setLabelProvider( new LabelProvider() );
    directionViewer.setContentProvider( new ArrayContentProvider() );
    directionViewer.setInput( STATIONING_DIRECTION.values() );

    final IObservableValue targetValue = ViewersObservables.observeSinglePostSelection( directionViewer );
    final IObservableValue targetEnablement = SWTObservables.observeEnabled( directionViewer.getControl() );

    final IObservableValue modelValue = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_DEFAULT_VALUE );
    final IObservableValue modelEnablement = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_ENABLEMENT );

    m_binding.bindValue( targetValue, modelValue );
    m_binding.bindValue( targetEnablement, modelEnablement );
  }

  private void createGeometryCheckControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    group.setText( "Geometry" );
    group.setLayout( new FillLayout() );
    GridLayoutFactory.swtDefaults().applyTo( group );

    m_geometryStatusComposite = new StatusComposite( group, SWT.NONE );
    m_geometryStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
  }

  private ImportAttributeInfo createAttributeControl( final String label, final String property, final Composite parent, final boolean optional )
  {
    new Label( parent, SWT.NONE ).setText( label );

    final ComboViewer viewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    final Combo combo = viewer.getCombo();
    combo.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    viewer.setLabelProvider( new DBFFieldLabelProvider() );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setComparator( new ViewerComparator() );

    final ImportAttributeInfo info = m_data.addAttributeInfo( property, viewer, optional );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_FIELD );

    final DataBinder binder = new DataBinder( target, model );
    if( !optional )
    {
      final String message = String.format( "'%s' is not set.", label );
      final ObjectNotSameValidator validator = new ObjectNotSameValidator( IStatus.ERROR, message, ImportAttributeInfo.FIELD_USE_DEFAULT );
      binder.addTargetAfterGetValidator( validator );
      binder.addModelBeforeSetValidator( validator );
    }
    binder.apply( m_binding );

    return info;
  }

  @Override
  public void update( )
  {
    ShapeFile shapeFile = null;
    try
    {
      /* Load shape */
      shapeFile = m_data.openShape();

      /* Check geometry */
      final IStatus geometryStatus = checkGeometry( shapeFile );
      m_geometryStatusComposite.setStatus( geometryStatus );

      /* Refresh combos */
      final IDBFField[] fields = shapeFile.getFields();
      final IDBFField[] fieldsWithNoData = (IDBFField[]) ArrayUtils.add( fields, ImportAttributeInfo.FIELD_USE_DEFAULT );
      final ImportAttributeInfo[] attributeInfos = m_data.getAttributeInfos();
      for( final ImportAttributeInfo info : attributeInfos )
      {
        final IDBFField oldField = info.getField();
        final ComboViewer viewer = info.getViewer();

        if( info.isOptional() )
        {
          viewer.setInput( fieldsWithNoData );
          info.setField( getField( fieldsWithNoData, oldField ) );
        }
        else
        {
          viewer.setInput( fields );
          info.setField( getField( fields, oldField ) );
        }
      }

      m_binding.updateTargets();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      setMessage( e.getLocalizedMessage(), ERROR );
    }
    catch( final DBaseException e )
    {
      e.printStackTrace();
      setMessage( e.getLocalizedMessage(), ERROR );
    }

    try
    {
      shapeFile.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      setMessage( e.getLocalizedMessage(), ERROR );
    }

    getContainer().updateButtons();
  }

  private IDBFField getField( final IDBFField[] fields, final IDBFField oldField )
  {
    if( oldField == null )
      return ImportAttributeInfo.FIELD_USE_DEFAULT;

    final String name = oldField.getName();
    for( final IDBFField field : fields )
    {
      if( field.getName().equals( name ) )
        return field;
    }

    return ImportAttributeInfo.FIELD_USE_DEFAULT;
  }

  private IStatus checkGeometry( final ShapeFile shapeFile )
  {
    final ShapeType shapeType = shapeFile.getShapeType();
    final String label = shapeType.getLabel();
    switch( shapeType )
    {
      case POLYLINE:
      case POLYLINEZ:
        return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, String.format( "%s", label ) );

      default:
        return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, String.format( "%s (must be a line geometry)", label ) );
    }
  }

  @Override
  public boolean isPageComplete( )
  {
    if( m_geometryStatusComposite == null )
      return false;

    final IStatus geometryStatus = m_geometryStatusComposite.getStatus();
    final boolean isGeometryOk = geometryStatus != null && geometryStatus.isOK();

    return super.isPageComplete() && isGeometryOk;
  }
}