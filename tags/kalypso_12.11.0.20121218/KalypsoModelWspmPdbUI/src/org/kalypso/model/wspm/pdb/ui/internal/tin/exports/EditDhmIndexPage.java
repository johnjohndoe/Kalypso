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
package org.kalypso.model.wspm.pdb.ui.internal.tin.exports;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.model.wspm.pdb.db.mapping.DhmIndex;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.ui.internal.tin.DhmIndexComposite;

/**
 * This page allows the editing of al {@link org.kalypso.model.wspm.pdb.db.mapping.DhmIndex}.
 *
 * @author Holger Albert
 */
public class EditDhmIndexPage extends WizardPage
{
  /**
   * The settings.
   */
  protected final PdbExportConnectionChooserData m_settingsData;

  /**
   * The dhm index composite.
   */
  protected DhmIndexComposite m_dhmIndexComposite;

  /**
   * The constructor.
   *
   * @param pageName
   *          The name of the page.
   * @param settingsData
   *          The settings.
   */
  public EditDhmIndexPage( final String pageName, final PdbExportConnectionChooserData settingsData )
  {
    this( pageName, null, null, settingsData );
  }

  /**
   * The constructor.
   *
   * @param pageName
   *          The name of the page.
   * @param title
   *          The title for this wizard page, or null if none.
   * @param titleImage
   *          The image descriptor for the title of this wizard page, or null if none.
   * @param settingsData
   *          The settings.
   */
  public EditDhmIndexPage( final String pageName, final String title, final ImageDescriptor titleImage, final PdbExportConnectionChooserData settingsData )
  {
    super( pageName, title, titleImage );

    m_settingsData = settingsData;
    m_dhmIndexComposite = null;

    setTitle( Messages.getString("EditDhmIndexPage_0") ); //$NON-NLS-1$
    setDescription( Messages.getString("EditDhmIndexPage_1") ); //$NON-NLS-1$
  }

  @Override
  public void createControl( final Composite parent )
  {
    /* Create the main composite. */
    final Composite main = new Composite( parent, SWT.NONE );
    final GridLayout mainLayout = new GridLayout( 1, false );
    mainLayout.marginHeight = 0;
    mainLayout.marginWidth = 0;
    main.setLayout( mainLayout );

    /* The dhm index to edit. */
    final DhmIndex dhmIndex = m_settingsData.getDhmIndex();

    /* Add the dhm index composite. */
    m_dhmIndexComposite = new DhmIndexComposite( main, SWT.NONE, dhmIndex, true, new DatabindingWizardPage( this, null ) );
    m_dhmIndexComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* React to changes to the data model from outside. */
    m_settingsData.addPropertyChangeListener( PdbExportConnectionChooserData.PROPERTY_DHM_INDEX, new PropertyChangeListener()
    {
      @Override
      public void propertyChange( final PropertyChangeEvent evt )
      {
        /* Parent is disposed. */
        if( main.isDisposed() )
          return;

        /* The dhm index to edit. */
        final DhmIndex newDhmIndex = m_settingsData.getDhmIndex();

        /* The dhm index composite needs to get the new object set. */
        if( m_dhmIndexComposite != null && !m_dhmIndexComposite.isDisposed() )
          m_dhmIndexComposite.dispose();

        /* Add the dhm index composite. */
        m_dhmIndexComposite = new DhmIndexComposite( main, SWT.NONE, newDhmIndex, true, new DatabindingWizardPage( EditDhmIndexPage.this, null ) );
        m_dhmIndexComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

        /* Do a layout. */
        main.layout();
      }
    } );

    /* Set the control to the page. */
    setControl( main );
  }
}