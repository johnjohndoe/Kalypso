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
package org.kalypso.ui.rrm.internal.cm.thiessen;

import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.core.layoutwizard.ILayoutPageContext;
import org.kalypso.core.layoutwizard.part.AbstractLayoutPart;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * A simple layout part that allows editing of options for the thiessen method.
 * 
 * @author Holger Albert
 */
public class ThiessenOptionsLayoutPart extends AbstractLayoutPart
{
  /**
   * The constructor.
   * 
   * @param id
   *          The id.
   * @param context
   *          The context.
   */
  public ThiessenOptionsLayoutPart( final String id, final ILayoutPageContext context )
  {
    super( id, context );
  }

  /**
   * The constructor.
   * 
   * @param id
   *          The id.
   * @param context
   *          The context.
   * @param selectionProvider
   *          The selection provider.
   */
  public ThiessenOptionsLayoutPart( final String id, final ILayoutPageContext context, final ISelectionProvider selectionProvider )
  {
    super( id, context, selectionProvider );
  }

  /**
   * @see org.kalypso.core.layoutwizard.ILayoutPart#init()
   */
  @Override
  public void init( )
  {
  }

  /**
   * @see org.kalypso.core.layoutwizard.ILayoutPart#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    /* Create the main composite. */
    final Composite main = toolkit.createComposite( parent, getStyle() );
    final GridLayout panelLayout = new GridLayout( 1, false );
    panelLayout.marginHeight = 0;
    panelLayout.marginWidth = 0;
    main.setLayout( panelLayout );
    main.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create the options section. */
    createOptionsSection( toolkit, main );

    /* Create the timeseries section. */
    createTimeseriesSection( toolkit, main );

    return main;
  }

  /**
   * @see org.kalypso.core.layoutwizard.ILayoutPart#dispose()
   */
  @Override
  public void dispose( )
  {
  }

  @SuppressWarnings("unused")
  private void createOptionsSection( final FormToolkit toolkit, final Composite parent )
  {
    // TODO At the moment this function does nothing. Perhaps later?
  }

  private void createTimeseriesSection( final FormToolkit toolkit, final Composite parent )
  {
    /* Header for table below. */
    final Section tableSection = toolkit.createSection( parent, Section.EXPANDED | Section.TITLE_BAR );
    tableSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    tableSection.setText( Messages.getString( "ThiessenWizardLayoutPart_1" ) ); //$NON-NLS-1$
  }
}