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
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.databinding.validation.ObjectNotSameValidator;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.dbf.DBFFieldLabelProvider;
import org.kalypso.shape.dbf.DBaseException;
import org.kalypso.shape.dbf.IDBFField;

/**
 * @author Gernot Belger
 */
public abstract class AbstractSelectAttributesPage extends WizardPage implements IUpdateable
{
  private final Map<ImportAttributeInfo< ? >, ComboViewer> m_infoComboMap = new HashMap<>();

  private StatusComposite m_geometryStatusComposite;

  private DatabindingWizardPage m_binding;

  private ImportAttributeInfo< ? >[] m_infos;

  protected AbstractSelectAttributesPage( final String pageName )
  {
    super( pageName );

    setTitle( Messages.getString( "AbstractSelectAttributesPage.0" ) ); //$NON-NLS-1$
  }

  @Override
  public final void createControl( final Composite parent )
  {
    m_binding = new DatabindingWizardPage( this, null );

    final Composite panel = new Composite( parent, SWT.NONE );
    setControl( panel );
    GridLayoutFactory.swtDefaults().applyTo( panel );

    createGeometryCheckControl( panel );

    final Group attributeGroup = new Group( panel, SWT.NONE );
    attributeGroup.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    attributeGroup.setText( Messages.getString( "AbstractSelectAttributesPage.1" ) ); //$NON-NLS-1$
    GridLayoutFactory.swtDefaults().numColumns( 3 ).equalWidth( false ).applyTo( attributeGroup );

    createAttributeControls( attributeGroup, m_binding );

    m_infos = m_infoComboMap.keySet().toArray( new ImportAttributeInfo< ? >[m_infoComboMap.size()] );
    setAttributeInfos( m_infos );

    setErrorMessage( null );
  }

  protected abstract void setAttributeInfos( ImportAttributeInfo< ? >[] infos );

  protected abstract void createAttributeControls( Composite parent, IDataBinding binding );

  private void createGeometryCheckControl( final Composite parent )
  {
    final Group group = new Group( parent, SWT.NONE );
    group.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    group.setText( Messages.getString( "AbstractSelectAttributesPage.2" ) ); //$NON-NLS-1$
    group.setLayout( new FillLayout() );
    GridLayoutFactory.swtDefaults().applyTo( group );

    m_geometryStatusComposite = new StatusComposite( group, SWT.NONE );
    m_geometryStatusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
  }

  protected final <T> ImportAttributeInfo<T> createAttributeControl( final String label, final String property, final Composite parent, final boolean optional )
  {
    new Label( parent, SWT.NONE ).setText( label );

    final ComboViewer viewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    final Combo combo = viewer.getCombo();
    combo.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    viewer.setLabelProvider( new DBFFieldLabelProvider() );
    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setComparator( new ViewerComparator() );

    final ImportAttributeInfo<T> info = new ImportAttributeInfo<>( property, optional );

    m_infoComboMap.put( info, viewer );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_FIELD );

    final DataBinder binder = new DataBinder( target, model );
    if( !optional )
    {
      final String message = String.format( Messages.getString( "AbstractSelectAttributesPage.3" ), label ); //$NON-NLS-1$
      final ObjectNotSameValidator validator = new ObjectNotSameValidator( IStatus.ERROR, message, ImportAttributeInfo.FIELD_USE_DEFAULT );
      binder.addTargetAfterGetValidator( validator );
      binder.addModelBeforeSetValidator( validator );
    }
    m_binding.bindValue( binder );

    return info;
  }

  @Override
  public final void update( )
  {
    try( ShapeFile shapeFile = openShape() )
    {
      /* Check geometry */
      final IStatus geometryStatus = checkGeometry( shapeFile );
      m_geometryStatusComposite.setStatus( geometryStatus );

      /* Refresh combos */
      final IDBFField[] fields = shapeFile.getFields();
      final IDBFField[] fieldsWithNoData = ArrayUtils.add( fields, ImportAttributeInfo.FIELD_USE_DEFAULT );
      for( final ImportAttributeInfo< ? > info : m_infos )
      {
        final IDBFField oldField = info.getField();
        final ComboViewer viewer = m_infoComboMap.get( info );

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

      m_binding.getBindingContext().updateTargets();
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

    getContainer().updateButtons();
  }

  protected abstract ShapeFile openShape( ) throws IOException, DBaseException;

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

  protected abstract IStatus checkGeometry( final ShapeFile shapeFile );

  @Override
  public final boolean isPageComplete( )
  {
    if( m_geometryStatusComposite == null )
      return false;

    final IStatus geometryStatus = m_geometryStatusComposite.getStatus();
    final boolean isGeometryOk = geometryStatus != null && geometryStatus.isOK();

    return super.isPageComplete() && isGeometryOk;
  }
}