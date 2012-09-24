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

import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.wspm.pdb.db.constants.WaterBodyConstants.STATIONING_DIRECTION;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.RankToStringConverter;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.StringToRankConverter;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.WaterBodyControl;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.shape.ShapeFile;
import org.kalypso.shape.ShapeType;
import org.kalypso.shape.dbf.DBaseException;

/**
 * @author Gernot Belger
 */
public class ImportWaterbodiesSelectAttributesPage extends AbstractSelectAttributesPage
{
  private final ImportWaterBodiesData m_data;

  protected ImportWaterbodiesSelectAttributesPage( final String pageName, final ImportWaterBodiesData data )
  {
    super( pageName );

    setDescription( Messages.getString( "ImportWaterbodiesSelectAttributesPage.0" ) ); //$NON-NLS-1$

    m_data = data;
  }

  @Override
  protected ShapeFile openShape( ) throws IOException, DBaseException
  {
    return m_data.openShape();
  }

  @Override
  protected void createAttributeControls( final Composite parent, final IDataBinding binding )
  {
    createCodeControl( parent );
    createNameControl( parent );
    createDescriptionControl( parent, binding );
    createDirectionControl( parent, binding );
    createRankControl( parent, binding );
  }

  @Override
  protected void setAttributeInfos( final ImportAttributeInfo< ? >[] infos )
  {
    m_data.setAttributeInfos( infos );
  }

  private void createCodeControl( final Composite parent )
  {
    createAttributeControl( Messages.getString( "ImportWaterbodiesSelectAttributesPage.1" ), WaterBody.PROPERTY_NAME, parent, false ); //$NON-NLS-1$

    /* No default control: code must be taken from shape */
    new Label( parent, SWT.NONE );
  }

  private void createNameControl( final Composite parent )
  {
    createAttributeControl( Messages.getString( "ImportWaterbodiesSelectAttributesPage.2" ), WaterBody.PROPERTY_LABEL, parent, true ); //$NON-NLS-1$

    /* No default control: code must be taken from shape */
    new Label( parent, SWT.NONE );
  }

  private void createDescriptionControl( final Composite parent, final IDataBinding binding )
  {
    final ImportAttributeInfo<String> info = createAttributeControl( Messages.getString( "ImportWaterbodiesSelectAttributesPage.3" ), WaterBody.PROPERTY_DESCRIPTION, parent, true ); //$NON-NLS-1$

    final Text text = new Text( parent, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    text.setMessage( Messages.getString( "ImportWaterbodiesSelectAttributesPage.4" ) ); //$NON-NLS-1$

    final ISWTObservableValue targetValue = SWTObservables.observeText( text, SWT.Modify );
    final ISWTObservableValue targetEnablement = SWTObservables.observeEnabled( text );

    final IObservableValue modelValue = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_DEFAULT_VALUE );
    final IObservableValue modelEnablement = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_ENABLEMENT );

    binding.bindValue( targetValue, modelValue );
    binding.bindValue( targetEnablement, modelEnablement );
  }

  private void createDirectionControl( final Composite parent, final IDataBinding binding )
  {
    final ImportAttributeInfo<STATIONING_DIRECTION> info = createAttributeControl( Messages.getString( "ImportWaterbodiesSelectAttributesPage.5" ), WaterBody.PROPERTY_DIRECTION_OF_STATIONING, parent, true ); //$NON-NLS-1$

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

    binding.bindValue( targetValue, modelValue );
    binding.bindValue( targetEnablement, modelEnablement );
  }

  private void createRankControl( final Composite parent, final IDataBinding binding )
  {
    final ImportAttributeInfo<Integer> info = createAttributeControl( Messages.getString( "ImportWaterbodiesSelectAttributesPage.6" ), WaterBody.PROPERTY_RANK, parent, true ); //$NON-NLS-1$

    info.setDefaultValue( WaterBody.DEFAULT_RANK );

    final ComboViewer rankViewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    rankViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    rankViewer.setLabelProvider( new LabelProvider() );
    rankViewer.setContentProvider( new ArrayContentProvider() );
    rankViewer.setInput( WaterBodyControl.RANK_INPUT );

    final IObservableValue targetValue = ViewersObservables.observeSinglePostSelection( rankViewer );
    final IObservableValue targetEnablement = SWTObservables.observeEnabled( rankViewer.getControl() );

    // REMARK: we explicitly need to declare the valueType here, else we get a BindingException, becsue the erased
    // property type is Object.
    final IObservableValue modelValue = BeanProperties.value( info.getClass(), ImportAttributeInfo.PROPERTY_DEFAULT_VALUE, Integer.class ).observe( info );

    final IObservableValue modelEnablement = BeansObservables.observeValue( info, ImportAttributeInfo.PROPERTY_ENABLEMENT );

    final DataBinder valueBinder = new DataBinder( targetValue, modelValue );
    valueBinder.setTargetToModelConverter( new StringToRankConverter( WaterBodyControl.RANK_INPUT ) );
    valueBinder.setModelToTargetConverter( new RankToStringConverter( WaterBodyControl.RANK_INPUT ) );
    binding.bindValue( valueBinder );

    binding.bindValue( targetEnablement, modelEnablement );
  }

  @Override
  protected IStatus checkGeometry( final ShapeFile shapeFile )
  {
    final ShapeType shapeType = shapeFile.getShapeType();
    final String label = shapeType.getLabel();
    switch( shapeType )
    {
      case POLYLINE:
      case POLYLINEZ:
        return new Status( IStatus.OK, WspmPdbUiPlugin.PLUGIN_ID, String.format( "%s", label ) ); //$NON-NLS-1$

      default:
        return new Status( IStatus.ERROR, WspmPdbUiPlugin.PLUGIN_ID, String.format( Messages.getString( "ImportWaterbodiesSelectAttributesPage.8" ), label ) ); //$NON-NLS-1$
    }
  }
}