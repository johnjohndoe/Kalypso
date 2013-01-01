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
package org.kalypso.ui.wizards.results;

import java.math.BigDecimal;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.project.Scenario1D2D;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * @author Thomas Jung
 */
public class TinResultThemeCreator extends AbstractThemeCreator
{
  private static final String LABEL_PROPERTY_FORMAT = String.format( "${property:%s#%s;-}", Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_PARAMETER.getNamespaceURI(), Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_PARAMETER.getLocalPart() ); //$NON-NLS-1$

  private static final String TIN_INFO_ID = "org.kalypso.ogc.gml.map.themeinfo.TriangulatedSurfaceThemeInfo"; //$NON-NLS-1$

  private final ResultAddLayerCommandData[] m_resultLayerCommandData = new ResultAddLayerCommandData[2];

  private IDocumentResultMeta m_documentResult = null;

  private boolean m_lineButtonChecked = true;

  private boolean m_polyButtonChecked = true;

  private final IFolder m_scenarioFolder;

  private ResultStyleComposite m_polyStyleComp;

  private ResultStyleComposite m_lineStyleComp;

  private final BigDecimal m_minValue;

  private final BigDecimal m_maxValue;

  private static final String UNIT_PROPERTY_FORMAT = String.format( "${property:%s#%s;-}", Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_UNIT.getNamespaceURI(), Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_UNIT.getLocalPart() ); //$NON-NLS-1$

  private final String THEME_INFO_ID = String.format( "%s?geometry=%s&format=%s: %s %s", TIN_INFO_ID, Kalypso1D2DSchemaConstants.TIN_RESULT_PROP_TIN, LABEL_PROPERTY_FORMAT, "%.2f", UNIT_PROPERTY_FORMAT ); //$NON-NLS-1$ //$NON-NLS-2$

  public TinResultThemeCreator( final IDocumentResultMeta documentResult, final IFolder scenarioFolder )
  {
    m_documentResult = documentResult;
    m_minValue = m_documentResult.getMinValue();
    m_maxValue = m_documentResult.getMaxValue();
    m_scenarioFolder = scenarioFolder;

    updateThemeCommandData();
  }

  @Override
  public Control createControl( final Composite parent )
  {
    // check if it is a TIN

    /* create control with selection buttons, style combo-boxes, edit button and delete button */
    final Composite buttonComp = new Composite( parent, SWT.NONE );
    buttonComp.setLayout( new GridLayout( 4, false ) );

    // selection button
    final Button lineButton = new Button( buttonComp, SWT.CHECK );
    lineButton.setText( Messages.getString( "org.kalypso.ui.wizards.results.TinResultThemeCreator.5" ) ); //$NON-NLS-1$
    lineButton.setToolTipText( Messages.getString( "org.kalypso.ui.wizards.results.TinResultThemeCreator.6" ) ); //$NON-NLS-1$
    lineButton.setSelection( m_lineButtonChecked );
    m_resultLayerCommandData[0].setSelected( m_lineButtonChecked );

    m_lineStyleComp = new ResultStyleComposite( buttonComp, m_scenarioFolder, "Line", m_minValue, m_maxValue, m_resultLayerCommandData[0] ); //$NON-NLS-1$
    m_lineStyleComp.setEnabled( m_lineButtonChecked );

    // selection button
    final Button polyButton = new Button( buttonComp, SWT.CHECK );
    polyButton.setText( Messages.getString( "org.kalypso.ui.wizards.results.TinResultThemeCreator.8" ) ); //$NON-NLS-1$
    polyButton.setToolTipText( Messages.getString( "org.kalypso.ui.wizards.results.TinResultThemeCreator.9" ) ); //$NON-NLS-1$
    polyButton.setSelection( m_polyButtonChecked );
    m_resultLayerCommandData[1].setSelected( m_lineButtonChecked );

    m_polyStyleComp = new ResultStyleComposite( buttonComp, m_scenarioFolder, "Polygon", m_minValue, m_maxValue, m_resultLayerCommandData[1] ); //$NON-NLS-1$
    m_polyStyleComp.setEnabled( m_polyButtonChecked );

    // call necessary in order to initialize the commands
    updateThemeCommandData();

    /* add listeners */

    // selection buttons
    lineButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings( "synthetic-access" )
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // update button selection and commands
        m_lineButtonChecked = lineButton.getSelection();
        m_lineStyleComp.setEnabled( lineButton.getSelection() );
        updateThemeCommandData();
        m_resultLayerCommandData[0].setSelected( m_lineButtonChecked );
      }
    } );

    polyButton.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings( "synthetic-access" )
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        // update button selection and commands
        m_polyButtonChecked = polyButton.getSelection();
        m_polyStyleComp.setEnabled( polyButton.getSelection() );
        updateThemeCommandData();
        m_resultLayerCommandData[1].setSelected( m_polyButtonChecked );
      }
    } );

    return buttonComp;
  }

  private void updateThemeCommandData( )
  {
    final String featurePath = ""; //$NON-NLS-1$

    /* Iso-Areas */
    if( m_polyButtonChecked == true )
    {
      final String type = NodeResultHelper.POLYGON_TYPE;

      // check, if there is a style already chosen, if not create one from default tamplate
      String styleLocation = null;
      if( m_polyStyleComp == null )
        styleLocation = getStyle( type );

      /* create the commands */
      if( m_resultLayerCommandData[1] != null )
        m_resultLayerCommandData[1].setValues( styleLocation );
      else
      {
        final String style = "tin" + type + "Style"; //$NON-NLS-1$ //$NON-NLS-2$

        m_resultLayerCommandData[1] = new ResultAddLayerCommandData( featurePath, style, styleLocation, m_scenarioFolder, type, m_documentResult );
        m_resultLayerCommandData[1].setSelected( true );

        m_resultLayerCommandData[1].setProperty( IKalypsoTheme.PROPERTY_DELETEABLE, Boolean.toString( true ) );
        m_resultLayerCommandData[1].setProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, THEME_INFO_ID );
        m_resultLayerCommandData[1].setProperty( ResultAddLayerCommandData.PROPERTY_RESULT_TYPE, type );
      }
    }

    /* Iso-Lines */
    if( m_lineButtonChecked == true )
    {
      final String type = "Line"; //$NON-NLS-1$

      String styleLocation = null;
      if( m_lineStyleComp == null )
        styleLocation = getStyle( type );

      /* create the commands */
      if( m_resultLayerCommandData[0] != null )
        m_resultLayerCommandData[0].setValues( styleLocation );
      else
      {
        final String style = "tin" + type + "Style"; //$NON-NLS-1$ //$NON-NLS-2$

        m_resultLayerCommandData[0] = new ResultAddLayerCommandData( featurePath, style, styleLocation, m_scenarioFolder, type, m_documentResult );
        m_resultLayerCommandData[0].setSelected( true );

        m_resultLayerCommandData[0].setProperty( IKalypsoTheme.PROPERTY_DELETEABLE, Boolean.toString( true ) );
        m_resultLayerCommandData[0].setProperty( IKalypsoTheme.PROPERTY_THEME_INFO_ID, THEME_INFO_ID );
        m_resultLayerCommandData[0].setProperty( ResultAddLayerCommandData.PROPERTY_RESULT_TYPE, type );
      }

    }
  }

  private String getStyle( final String type )
  {
    final Scenario1D2D scenario = new Scenario1D2D( m_scenarioFolder );
    final IFolder resultsFolder = scenario.getResultsFolder();
    final String resFolder = resultsFolder.getFullPath().toPortableString();

    final IFolder stylesFolder = scenario.getStylesFolder();
    final String defaultPath = stylesFolder.getFullPath().toPortableString();
    final String relativePathTo = FileUtilities.getRelativePathTo( resFolder, defaultPath );

    /* default location depending on type */
    final IFolder sldFolder = stylesFolder.getFolder( type );

    final String sldFileName = "default" + type + m_documentResult.getDocumentType().name() + "Style.sld"; //$NON-NLS-1$ //$NON-NLS-2$
    final IPath stylePath = new Path( ".." ).append( relativePathTo ).append( type ).append( sldFileName ); //$NON-NLS-1$
    final String styleLocation = stylePath.toPortableString();

    /* create sld file if it does not already exist */
    final IFile styleFile = sldFolder.getFile( sldFileName );
    if( styleFile.exists() == false )
      ResultSldHelper.processStyle( styleFile, sldFolder, type, m_minValue, m_maxValue );

    return styleLocation;
  }

  @Override
  public ResultAddLayerCommandData[] getThemeCommandData( )
  {
    if( m_resultLayerCommandData == null )
      return null;

    if( m_resultLayerCommandData[0].isSelected() == true && m_resultLayerCommandData[1].isSelected() == true )
      return m_resultLayerCommandData;

    if( m_resultLayerCommandData[0].isSelected() == true && m_resultLayerCommandData[1].isSelected() == false )
      return new ResultAddLayerCommandData[] { m_resultLayerCommandData[0] };

    if( m_resultLayerCommandData[0].isSelected() == false && m_resultLayerCommandData[1].isSelected() == true )
      return new ResultAddLayerCommandData[] { m_resultLayerCommandData[1] };

    return null;
  }
}