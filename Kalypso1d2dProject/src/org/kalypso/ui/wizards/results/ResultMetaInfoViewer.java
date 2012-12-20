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
import java.text.DateFormat;
import java.util.Date;
import java.util.Map;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormText;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 */
public class ResultMetaInfoViewer extends Viewer
{
  /*
   * fonts
   */
  // FIXME never disposed; use JFaceResources instead!
  private final Font fTextHeader;

  private final Font fTextNormal;

  private final Group m_panel;

  private Object m_input;

  private FormText m_textPanel;

  private final IThemeConstructionFactory m_factory;

  public ResultMetaInfoViewer( final Composite parent, final int style, final IThemeConstructionFactory factory )
  {
    m_panel = new Group( parent, style );
    m_panel.setLayout( new GridLayout() );
    fTextHeader = new Font( parent.getDisplay(), "Tahoma", 10, SWT.BOLD ); //$NON-NLS-1$
    fTextNormal = new Font( parent.getDisplay(), "Tahoma", 8, SWT.NONE ); //$NON-NLS-1$
    parent.getDisplay();

    m_factory = factory;
  }

  @Override
  public Control getControl( )
  {
    return m_panel;
  }

  @Override
  public Object getInput( )
  {
    return m_input;
  }

  @Override
  public ISelection getSelection( )
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void refresh( )
  {
    /* Empty old stuff */
    final Control[] children = m_panel.getChildren();
    for( final Control control : children )
      control.dispose();

    m_textPanel = new FormText( m_panel, SWT.WRAP | SWT.READ_ONLY );
    m_textPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_panel.setText( Messages.getString( "org.kalypso.ui.wizards.results.ResultMetaInfoViewer.0" ) ); //$NON-NLS-1$
    m_textPanel.setFont( "header", fTextHeader ); //$NON-NLS-1$
    m_textPanel.setFont( "text", fTextNormal ); //$NON-NLS-1$

    if( m_input instanceof IResultMeta )
    {
      final IResultMeta result = (IResultMeta)m_input;

      // special result data
      if( m_factory != null )
      {
        final IResultThemeConstructor createThemeCreator = m_factory.createThemeConstructor( result );
        final Composite buttonControl = createThemeCreator.createControl( m_panel );
        if( buttonControl != null )
          buttonControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
      }

      final String infoText = getInformationText( result );
      m_textPanel.setText( infoText, true, false );
    }
    m_panel.layout( true );
  }

  // FIXME: move into a separate builder class, we need a much better abstraction here...
  public static String getInformationText( final IResultMeta result )
  {
    if( result == null )
      return Messages.getString( "org.kalypso.ui.wizards.results.ResultMetaInfoViewer.5" ); //$NON-NLS-1$

    final StringBuffer buf = new StringBuffer();

    /* possible entries */
    // final String scenarioName;
    // final String scenarioDescription;
    String calcUnitName = null;
    // String calcUnitDescription = null;
    String calcStart = null;
    String calcEnd = null;

    // String stepName = null;
    // String stepDescription = null;
    String stepType = null;
    String stepTime = null;

    // String docName;
    // String docDescription;
    String docType = null;
    BigDecimal docMin = null;
    BigDecimal docMax = null;

    // TODO: u‰argh! Lots of copy/paste code....!
    // final IScenarioResultMeta scenarioResult = getScenarioResultMeta( result );
    // if( scenarioResult != null )
    // {
    // scenarioName = scenarioResult.getName();
    // scenarioDescription = scenarioResult.getDescription();
    // }
    // else
    // {
    // scenarioName = null;
    // scenarioDescription = null;
    // }

    final DateFormat dateFormat = DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.LONG );
    // dateFormat.setTimeZone( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
    dateFormat.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );

    final ICalcUnitResultMeta calcUnitResult = ResultMeta1d2dHelper.getCalcUnitResultMeta( result );
    if( calcUnitResult != null )
    {
      calcUnitName = calcUnitResult.getName();
      // calcUnitDescription = calcUnitResult.getDescription();

      final Date calcStartTime = calcUnitResult.getCalcStartTime();
      final Date calcEndTime = calcUnitResult.getCalcEndTime();
      calcStart = calcStartTime == null ? "-" : dateFormat.format( calcStartTime ); //$NON-NLS-1$
      calcEnd = calcEndTime == null ? "-" : dateFormat.format( calcEndTime ); //$NON-NLS-1$
    }

    final IStepResultMeta stepResult = getStepResultMeta( result );
    if( stepResult != null )
    {
      // stepName = stepResult.getName();
      // stepDescription = stepResult.getDescription();
      stepType = stepResult.getStepType().toString();
      final Date stepResultTime = stepResult.getStepTime();
      stepTime = stepResultTime == null ? "-" : dateFormat.format( stepResultTime ); //$NON-NLS-1$
    }

    IDocumentResultMeta docResult = null;
    if( result instanceof IDocumentResultMeta )
    {
      docResult = (IDocumentResultMeta)result;

      // get infos of the selected document
      // docName = docResult.getName();
      // docDescription = docResult.getDescription();
      docType = docResult.getDocumentType().toString();
      // if( docResult.getMinValue() != null )
      // {
      // docMin = docResult.getMinValue().toString();
      // }
      //
      // if( docResult.getMaxValue() != null )
      // {
      // docMax = docResult.getMaxValue().toString();
      // }
    }

    /* make string buffer */

    buf.append( "<form>" ); //$NON-NLS-1$

    // Scenario
    /*
     * if( scenarioResult != null ) { buf.append( "<p>" ); buf.append( "<span color=\"header\" font=\"header\">" +
     * "Szenario: " + scenarioName + "</span>" ); buf.append( "</p>" );
     * buf.append( "<p>" ); buf.append( scenarioDescription ); buf.append( "</p>" ); }
     */
    // CalcUnit
    if( calcUnitResult != null )
    {
      buf.append( "<p>" ); //$NON-NLS-1$
      buf.append( "<b>Teilmodell " + calcUnitName + "</b>" ); //$NON-NLS-1$ //$NON-NLS-2$
      buf.append( "</p>" ); //$NON-NLS-1$
      //
      // buf.append( "<p>" );
      // buf.append( calcUnitDescription );
      // buf.append( "</p>" );

      buf.append( "<p>" ); //$NON-NLS-1$
      buf.append( "<b>Datum der Berechnung:</b>" ); //$NON-NLS-1$
      buf.append( "</p>" ); //$NON-NLS-1$

      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"Beginn:\">" + calcStart + "</li>" ); //$NON-NLS-1$ //$NON-NLS-2$
      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"Ende:\">" + calcEnd + "</li>" ); //$NON-NLS-1$ //$NON-NLS-2$
      buf.append( "<br/>\n\n" ); //$NON-NLS-1$
    }

    // Step
    if( stepResult != null )
    {
      buf.append( "<p>" ); //$NON-NLS-1$
      buf.append( "<b>Zeitschrittart:</b>" ); //$NON-NLS-1$
      buf.append( "</p>" ); //$NON-NLS-1$

      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"" + stepType + "\"></li>" ); //$NON-NLS-1$ //$NON-NLS-2$

      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"zum Zeitpunkt:\">" + stepTime + "</li>" ); //$NON-NLS-1$ //$NON-NLS-2$

      buf.append( "<br/>" ); //$NON-NLS-1$
    }

    // Document
    if( docResult != null )
    {
      buf.append( "<p>" ); //$NON-NLS-1$
      buf.append( "<b>Datentyp: </b>" ); //$NON-NLS-1$
      buf.append( "</p>" ); //$NON-NLS-1$

      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"" + docType + "\"></li>" ); //$NON-NLS-1$ //$NON-NLS-2$
      boolean bDone = false;

      for( final String resultType : NodeResultHelper.NodeStyleTypes )
      {
        BigDecimal minValueForType = null;
        BigDecimal maxValueForType = null;
        if( resultType.equals( NodeResultHelper.WAVE_DIRECTION_TYPE ) )
          continue;

        try
        {
          minValueForType = docResult.getMinValueForType( resultType );
          docMin = minValueForType;
          maxValueForType = docResult.getMaxValueForType( resultType );
          docMax = maxValueForType;
        }
        catch( final Exception e )
        {
          System.out.println();
          continue;
        }

        if( docMax != null )
        {
          buf.append( "<li style=\"text\" bindent=\"40\" indent=\"190\" value=\"maximaler " + resultType + " Wert:\">" + docMax + "</li>" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        if( docMin != null )
        {
          buf.append( "<li style=\"text\" bindent=\"40\" indent=\"190\" value=\"minimaler " + resultType + " Wert:\">" + docMin + "</li>" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        bDone = true;
        /*
         * set actual min/max settings also in the helper map
         */
        final String sourceFile = docResult.getFullPath().toOSString();
        final int beginIndex = sourceFile.indexOf( ResultMeta1d2dHelper.TIME_STEP_PREFIX ) + ResultMeta1d2dHelper.TIME_STEP_PREFIX.length();
        final String stepNameStr = sourceFile.substring( beginIndex, beginIndex + 16 );
        final Map<String, Object> m_mapSldSettingsIntern = NodeResultHelper.getSldSettingsMapForStep( stepNameStr );

        final Double minValueForTypeAsDouble = minValueForType == null ? null : minValueForType.doubleValue();
        final Double maxValueForTypeAsDouble = maxValueForType == null ? null : maxValueForType.doubleValue();

        m_mapSldSettingsIntern.put( NodeResultHelper.VALUE_MIN_PREFIX + resultType.toLowerCase(), minValueForTypeAsDouble );
        m_mapSldSettingsIntern.put( NodeResultHelper.VALUE_MAX_PREFIX + resultType.toLowerCase(), maxValueForTypeAsDouble );
      }
      if( !bDone )
      {
        docMin = null;
        docMax = null;
        try
        {
          docMin = docResult.getMinValue();
          docMax = docResult.getMaxValue();
        }
        catch( final Exception e )
        {
          System.out.println();
        }
        if( docMax != null )
        {
          buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"maximaler Wert:\">" + docMax + "</li>" ); //$NON-NLS-1$ //$NON-NLS-2$
        }
        if( docMin != null )
        {
          buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"minimaler Wert:\">" + docMin + "</li>" ); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
    }

    buf.append( "</form>" ); //$NON-NLS-1$

    return buf.toString();
  }

  /**
   * gets the StepResultMeta as the papa of all documents (except tin_terrain)
   */
  private static IStepResultMeta getStepResultMeta( final IResultMeta result )
  {
    if( result instanceof IStepResultMeta )
      return (IStepResultMeta)result;
    else
    {
      final IResultMeta parent = result.getOwner();
      if( parent != null )
      {
        return getStepResultMeta( parent );
      }
    }
    return null;
  }

  @Override
  public void setInput( final Object input )
  {
    m_input = input;

    refresh();
  }

  @Override
  public void setSelection( final ISelection selection, final boolean reveal )
  {
    throw new UnsupportedOperationException();
  }
}