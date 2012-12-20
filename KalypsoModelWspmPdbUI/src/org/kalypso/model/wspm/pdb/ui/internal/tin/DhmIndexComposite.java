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
package org.kalypso.model.wspm.pdb.ui.internal.tin;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * This composite allows the editing of {@link org.kalypso.model.wspm.pdb.db.mapping.DhmIndex}.
 * 
 * @author Holger Albert
 */
public class DhmIndexComposite extends Composite
{
  /**
   * The dhm index.
   */
  private DhmIndex m_dhmIndex;

  /**
   * True, if the controls should be editable.
   */
  private final boolean m_editable;

  /**
   * The data binding.
   */
  private final IDataBinding m_dataBinding;

  /**
   * The constructor.
   * 
   * @param parent
   *          A widget which will be the parent of the new instance (cannot be null).
   * @param style
   *          The style of widget to construct.
   * @param dhmIndex
   *          The dhm index to edit.
   * @param editable
   *          True, if the controls should be editable.
   */
  public DhmIndexComposite( final Composite parent, final int style, final DhmIndex dhmIndex, final boolean editable, final IDataBinding dataBinding )
  {
    super( parent, style );

    /* Initialize. */
    m_dhmIndex = dhmIndex != null ? dhmIndex : new DhmIndex();
    m_editable = editable;
    m_dataBinding = dataBinding;

    /* Create the controls. */
    createControls();
  }

  @Override
  public void setLayout( final Layout layout )
  {
    /* Ignore user set layouts, only layout datas are permitted. */
  }

  @Override
  public void dispose( )
  {
    m_dhmIndex = null;

    super.dispose();
  }

  /**
   * This function creates the controls.
   */
  private void createControls( )
  {
    /* Create the layout. */
    final GridLayout parentLayout = new GridLayout( 1, false );
    parentLayout.marginHeight = 0;
    parentLayout.marginWidth = 0;
    super.setLayout( parentLayout );

    /* Create the main composite. */
    final Composite main = new Composite( this, SWT.NONE );
    main.setLayout( new GridLayout( 2, false ) );
    main.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create the controls. */
    createTextControl( main, m_dataBinding, Messages.getString("DhmIndexComposite_0"), DhmIndex.PROPERTY_FILENAME, false ); //$NON-NLS-1$
    createTextControl( main, m_dataBinding, Messages.getString("DhmIndexComposite_1"), DhmIndex.PROPERTY_MIMETYPE, false ); //$NON-NLS-1$
    createTextControl( main, m_dataBinding, Messages.getString("DhmIndexComposite_2"), DhmIndex.PROPERTY_DESCRIPTION, m_editable ); //$NON-NLS-1$
    createTextControl( main, m_dataBinding, Messages.getString("DhmIndexComposite_3"), DhmIndex.PROPERTY_EDITINGUSER, m_editable ); //$NON-NLS-1$
    createDateControl( main, m_dataBinding, Messages.getString("DhmIndexComposite_4"), DhmIndex.PROPERTY_MEASUREMENTDATE, m_editable ); //$NON-NLS-1$
    createTextControl( main, m_dataBinding, Messages.getString("DhmIndexComposite_5"), DhmIndex.PROPERTY_MEASUREMENTACCURACY, m_editable ); //$NON-NLS-1$
    createTextControl( main, m_dataBinding, Messages.getString("DhmIndexComposite_6"), DhmIndex.PROPERTY_SOURCE, m_editable ); //$NON-NLS-1$
    createTextControl( main, m_dataBinding, Messages.getString("DhmIndexComposite_7"), DhmIndex.PROPERTY_EDITOR, m_editable ); //$NON-NLS-1$
    createTextControl( main, m_dataBinding, Messages.getString("DhmIndexComposite_8"), DhmIndex.PROPERTY_COPYRIGHT, m_editable ); //$NON-NLS-1$
    // createTextControl( main, m_dataBinding, "ID", DhmIndex.PROPERTY_ID, m_editable );
    // createTextControl( main, m_dataBinding, "Name", DhmIndex.PROPERTY_NAME, m_editable );
    // createDateControl( main, m_dataBinding, "Erstellungsdatum", DhmIndex.PROPERTY_CREATIONDATE, m_editable );
    // createDateControl( main, m_dataBinding, "Änderungsdatum", DhmIndex.PROPERTY_EDITINGDATE, m_editable );
    // createLocationControl( main, m_dataBinding, "Ort", m_editable );
    // createSridControl( main, m_dataBinding, "Koordinaten-System", m_editable );
  }

  private void createTextControl( final Composite main, final IDataBinding dataBinding, final String displayLabel, final String property, final boolean editable )
  {
    final Label label = new Label( main, SWT.NONE );
    label.setLayoutData( new GridData( SWT.FILL, SWT.FILL, false, false ) );
    label.setText( displayLabel );

    final Text text = new Text( main, SWT.BORDER );
    text.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    text.setEnabled( editable );

    final ISWTObservableValue targetValue = SWTObservables.observeText( text, SWT.Modify );
    final IObservableValue modelValue = BeansObservables.observeValue( m_dhmIndex, property );
    dataBinding.bindValue( targetValue, modelValue );
  }

  private void createDateControl( final Composite main, final IDataBinding dataBinding, final String displayLabel, final String property, final boolean editable )
  {
    final Label label = new Label( main, SWT.NONE );
    label.setLayoutData( new GridData( SWT.FILL, SWT.FILL, false, false ) );
    label.setText( displayLabel );

    final DateTime dateTime = new DateTime( main, SWT.DATE | SWT.DROP_DOWN | SWT.BORDER );
    dateTime.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    dateTime.setEnabled( editable );

    final ISWTObservableValue targetValue = SWTObservables.observeSelection( dateTime );
    final IObservableValue modelValue = BeansObservables.observeValue( m_dhmIndex, property );
    dataBinding.bindValue( targetValue, modelValue );
  }
}