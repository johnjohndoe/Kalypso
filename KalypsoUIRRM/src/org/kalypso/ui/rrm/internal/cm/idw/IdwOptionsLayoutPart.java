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
package org.kalypso.ui.rrm.internal.cm.idw;

import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.contribs.eclipse.swt.events.IntegerModifyListener;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.core.layoutwizard.ILayoutPageContext;
import org.kalypso.core.layoutwizard.part.AbstractLayoutPart;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.ui.rrm.internal.cm.view.LinearSumBean;
import org.kalypso.ui.rrm.internal.i18n.Messages;

/**
 * A simple layout part that allows editing of options for the idw method.
 * 
 * @author Holger Albert
 */
public class IdwOptionsLayoutPart extends AbstractLayoutPart
{
  /**
   * The default number of stations to use.
   */
  private static final Integer DEFAULT_MAX_STATIONS = new Integer( 1 );

  /**
   * The description pattern.
   */
  private static final String DESCRIPTION_PATTERN = Messages.getString( "IdwOptionsLayoutPart_0" ); //$NON-NLS-1$

  /**
   * The maximum number of stations to use.
   */
  protected Integer m_maxStations;

  /**
   * The constructor.
   * 
   * @param id
   *          The id.
   * @param context
   *          The context.
   */
  public IdwOptionsLayoutPart( final String id, final ILayoutPageContext context )
  {
    super( id, context );

    m_maxStations = null;
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
  public IdwOptionsLayoutPart( final String id, final ILayoutPageContext context, final ISelectionProvider selectionProvider )
  {
    super( id, context, selectionProvider );

    m_maxStations = null;
  }

  /**
   * @see org.kalypso.core.layoutwizard.ILayoutPart#init()
   */
  @Override
  public void init( )
  {
    final ILayoutPageContext context = getContext();
    final IdwGeneratorWizard wizard = (IdwGeneratorWizard) context.getPage().getWizard();
    final LinearSumBean generator = wizard.getGenerator();
    final String comment = (String) generator.getProperty( ILinearSumGenerator.PROPERTY_COMMENT );
    if( comment == null || comment.length() == 0 )
    {
      m_maxStations = DEFAULT_MAX_STATIONS;
      return;
    }

    final Pattern pattern = Pattern.compile( DESCRIPTION_PATTERN );
    final Matcher matcher = pattern.matcher( comment );
    if( !matcher.matches() )
    {
      m_maxStations = DEFAULT_MAX_STATIONS;
      return;
    }

    final String group = matcher.group( 1 );
    final Integer maxStations = NumberUtils.parseQuietInteger( group );
    if( maxStations == null )
    {
      m_maxStations = DEFAULT_MAX_STATIONS;
      return;
    }

    m_maxStations = maxStations;
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
    m_maxStations = null;
  }

  private void createOptionsSection( final FormToolkit toolkit, final Composite parent )
  {
    /* Create the options section. */
    final Section optionsSection = toolkit.createSection( parent, Section.EXPANDED | Section.TITLE_BAR );
    optionsSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    optionsSection.setText( Messages.getString( "IdwOptionsLayoutPart_1" ) ); //$NON-NLS-1$

    /* The content. */
    final Composite content = toolkit.createComposite( parent, SWT.NONE );
    content.setLayout( new GridLayout( 2, false ) );
    content.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    /* Create a label. */
    final Label maxStationsLabel = toolkit.createLabel( content, Messages.getString( "IdwOptionsLayoutPart_2" ), SWT.NONE ); //$NON-NLS-1$
    maxStationsLabel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, false, false ) );
    maxStationsLabel.setToolTipText( Messages.getString( "IdwOptionsLayoutPart_3" ) ); //$NON-NLS-1$

    /* Create a text field. */
    final Text maxStationsText = toolkit.createText( content, "", SWT.BORDER | SWT.TRAIL ); //$NON-NLS-1$
    maxStationsText.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    maxStationsText.setToolTipText( Messages.getString( "IdwOptionsLayoutPart_3" ) ); //$NON-NLS-1$
    maxStationsText.setMessage( Messages.getString( "IdwOptionsLayoutPart_4" ) ); //$NON-NLS-1$
    maxStationsText.setText( String.format( Locale.PRC, "%d", m_maxStations ) ); //$NON-NLS-1$

    /* Some colors for validating the input. */
    final Display display = getContext().getShell().getDisplay();
    final Color badColor = display.getSystemColor( SWT.COLOR_RED );
    final Color goodColor = display.getSystemColor( SWT.COLOR_BLACK );

    /* Add a listener. */
    maxStationsText.addModifyListener( new IntegerModifyListener( goodColor, badColor ) );

    /* Add a listener. */
    maxStationsText.addModifyListener( new ModifyListener()
    {
      /**
       * @see org.eclipse.swt.events.ModifyListener#modifyText(org.eclipse.swt.events.ModifyEvent)
       */
      @Override
      public void modifyText( final ModifyEvent e )
      {
        final Text source = (Text) e.getSource();
        final String maxStations = source.getText();
        m_maxStations = NumberUtils.parseQuietInteger( maxStations );
      }
    } );

    /* Do a layout. */
    optionsSection.layout();
  }

  private void createTimeseriesSection( final FormToolkit toolkit, final Composite parent )
  {
    /* Header for table below. */
    final Section tableSection = toolkit.createSection( parent, Section.EXPANDED | Section.TITLE_BAR );
    tableSection.setText( Messages.getString( "IdwOptionsLayoutPart_5" ) ); //$NON-NLS-1$
    tableSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
  }

  /**
   * This function returns the maximum number of stations to use.
   * 
   * @return The maximum number of stations to use.
   */
  public Integer getMaxStations( )
  {
    return m_maxStations;
  }
}