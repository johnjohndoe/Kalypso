/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.risk.model.simulation.statistics;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.contribs.eclipse.jface.viewers.FileLabelProvider;

/**
 * @author Gernot Belger
 */
public class StatisticCalculationShapePage extends WizardPage
{
  private final StatisticCalculationData m_data;

  private DatabindingWizardPage m_binding;

  public StatisticCalculationShapePage( final String pageName, final StatisticCalculationData data )
  {
    super( pageName );

    m_data = data;

    setTitle( "Additional Categorization" );
    setDescription( "Choose a shape file to additionally group the statistic results. Choose <None> for no additional categories." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    final Group panel = new Group( parent, SWT.NONE );
    setControl( panel );

    m_binding = new DatabindingWizardPage( this, null );

    panel.setText( "Available Shape Files" );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( panel );

    createShapeFileControl( panel );
    createShapeAttributeControl( panel );
  }

  private void createShapeFileControl( final Composite parent )
  {
    final String tooltip = "If set, the damage values get additionally grouped by the areas defined in the shape file.";

    final Label label = new Label( parent, SWT.NONE );
    label.setText( "Shape File" );
    label.setToolTipText( tooltip );

    final ComboViewer comboViewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    comboViewer.getControl().setToolTipText( tooltip );
    comboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    comboViewer.setContentProvider( new ArrayContentProvider() );
    comboViewer.setLabelProvider( new FileLabelProvider() );

    comboViewer.setInput( m_data.getAvailableShapeFiles() );

    /* Binding */
    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( comboViewer );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, StatisticCalculationData.PROPERTY_SELECTED_SHAPE );
    m_binding.bindValue( targetSelection, modelSelection );
  }

  private void createShapeAttributeControl( final Composite parent )
  {
    final String tooltip = "The Value of this shape attribute is shown as group name.";

    final Label label = new Label( parent, SWT.NONE );
    label.setText( "Shape Attribute" );
    label.setToolTipText( tooltip );

    final ComboViewer comboViewer = new ComboViewer( parent, SWT.DROP_DOWN | SWT.READ_ONLY );
    comboViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    comboViewer.getControl().setToolTipText( tooltip );

    comboViewer.setContentProvider( new ArrayContentProvider() );
    comboViewer.setLabelProvider( new LabelProvider() );

    /* Binding */
    final IObservableValue targetInput = ViewersObservables.observeInput( comboViewer );
    final IObservableValue modelInput = BeansObservables.observeValue( m_data, StatisticCalculationData.PROPERTY_SHAPE_ATTRIBUTES );
    final IValidator finishValidator = new ShapeAttributesValidValidator();
    m_binding.bindValue( targetInput, modelInput, finishValidator, finishValidator );

    final IViewerObservableValue targetSelection = ViewersObservables.observeSinglePostSelection( comboViewer );
    final IObservableValue modelSelection = BeansObservables.observeValue( m_data, StatisticCalculationData.PROPERTY_SELECTED_ATTRIBUTE );
    m_binding.bindValue( targetSelection, modelSelection );
  }
}