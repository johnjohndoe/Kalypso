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

import java.text.DateFormat;
import java.text.SimpleDateFormat;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.forms.widgets.FormText;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * @author Thomas Jung
 * 
 */
public class ResultMetaInfoViewer extends Viewer
{

  private static final DateFormat INFO_DF = SimpleDateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.LONG );

  /*
   * fonts
   */
  private final Font fTextHeader = new Font( Display.getDefault(), "Tahoma", 10, SWT.BOLD );

  private final Font fTextNormal = new Font( Display.getDefault(), "Tahoma", 8, SWT.NONE );

  private final Group m_panel;

  private Object m_input;

  private FormText m_textPanel;

  private final IThemeConstructionFactory m_factory;

  public ResultMetaInfoViewer( final Composite parent, final int style, IThemeConstructionFactory factory )
  {
    m_panel = new Group( parent, style );
    m_panel.setLayout( new GridLayout() );

    m_factory = factory;

  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getControl()
   */
  @Override
  public Control getControl( )
  {
    return m_panel;
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getInput()
   */
  @Override
  public Object getInput( )
  {
    return m_input;
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#getSelection()
   */
  @Override
  public ISelection getSelection( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#refresh()
   */
  @Override
  public void refresh( )
  {
    /* Empty old stuff */
    final Control[] children = m_panel.getChildren();
    for( Control control : children )
      control.dispose();

    m_textPanel = new FormText( m_panel, SWT.WRAP | SWT.READ_ONLY );
    m_textPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_panel.setText( "Information" );
    m_textPanel.setFont( "header", fTextHeader );
    m_textPanel.setFont( "text", fTextNormal );

    if( m_input instanceof IResultMeta )
    {
      final IResultMeta result = (IResultMeta) m_input;

      // special result data
      if( m_factory != null )
      {
        IResultThemeConstructor createThemeCreator = m_factory.createThemeConstructor( result );
        final Composite buttonControl = createThemeCreator.createControl( m_panel );
        if( buttonControl != null )
          buttonControl.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
      }

      String infoText = getInformationText( result );
      m_textPanel.setText( infoText, true, false );
    }
    m_panel.layout( true );
  }

  private String getInformationText( final IResultMeta result )
  {
    if( result == null )
      return "kein Ergebnis selektiert";

    IScenarioResultMeta scenarioResult = null;
    ICalcUnitResultMeta calcUnitResult = null;
    IStepResultMeta stepResult = null;
    IDocumentResultMeta docResult = null;

    StringBuffer buf = new StringBuffer();

    /* possible entries */
    String scenarioName = null;
    String scenarioDescription = null;

    String calcUnitName = null;
    String calcUnitDescription = null;
    String calcStart = null;
    String calcEnd = null;

    String stepName = null;
    String stepDescription = null;
    String stepType = null;
    String stepTime = null;
    String stepNumber = null;

    String docName;
    String docDescription;
    String docType = null;
    String docMin = null;
    String docMax = null;

    if( result instanceof ICalcUnitResultMeta )
    {
      // get info of the parent
      scenarioResult = getScenarioResultMeta( result );
      if( scenarioResult != null )
      {
        scenarioName = scenarioResult.getName();
        scenarioDescription = scenarioResult.getDescription();
      }

      // get selection
      calcUnitResult = (ICalcUnitResultMeta) result;

      // get infos about the selected calc unit
      calcUnitName = calcUnitResult.getName();
      calcUnitDescription = calcUnitResult.getDescription();
      calcStart = INFO_DF.format( calcUnitResult.getCalcStartTime() );
      calcEnd = INFO_DF.format( calcUnitResult.getCalcEndTime() );

    }
    else if( result instanceof IStepResultMeta )
    {
      // get parents
      scenarioResult = getScenarioResultMeta( result );

      if( scenarioResult != null )
      {
        scenarioName = scenarioResult.getName();
        scenarioDescription = scenarioResult.getDescription();
      }

      calcUnitResult = getCalcUnitResultMeta( result );
      if( calcUnitResult != null )
      {
        calcUnitName = calcUnitResult.getName();
        calcUnitDescription = calcUnitResult.getDescription();
        calcStart = INFO_DF.format( calcUnitResult.getCalcStartTime() );
        calcEnd = INFO_DF.format( calcUnitResult.getCalcEndTime() );
      }

      // get selection
      stepResult = (IStepResultMeta) result;

      // get infos of the selected time step
      stepName = stepResult.getName();
      stepDescription = stepResult.getDescription();
      stepType = stepResult.getStepType().toString();
      stepTime = INFO_DF.format( stepResult.getStepTime() );
      stepNumber = ((Integer) stepResult.getStepNumber()).toString();
      // TODO: create a link to status
    }
    else if( result instanceof IDocumentResultMeta )
    {
      // get info of the parents
      scenarioResult = getScenarioResultMeta( result );
      if( scenarioResult != null )
      {
        scenarioName = scenarioResult.getName();
        scenarioDescription = scenarioResult.getDescription();
      }

      calcUnitResult = getCalcUnitResultMeta( result );
      if( calcUnitResult != null )
      {
        calcUnitName = calcUnitResult.getName();
        calcUnitDescription = calcUnitResult.getDescription();
        calcStart = INFO_DF.format( calcUnitResult.getCalcStartTime() );
        calcEnd = INFO_DF.format( calcUnitResult.getCalcEndTime() );
      }

      stepResult = getStepResultMeta( result );
      if( stepResult != null )
      {
        stepName = stepResult.getName();
        stepDescription = stepResult.getDescription();
        stepType = stepResult.getStepType().toString();
        stepTime = INFO_DF.format( stepResult.getStepTime() );
        stepNumber = ((Integer) stepResult.getStepNumber()).toString();
      }

      // get selection
      docResult = (IDocumentResultMeta) result;

      // get infos of the selected document
      docName = docResult.getName();
      docDescription = docResult.getDescription();
      docType = docResult.getDocumentType().toString();
      if( docResult.getMinValue() != null )
      {
        docMin = docResult.getMinValue().toString();
      }

      if( docResult.getMaxValue() != null )
      {
        docMax = docResult.getMaxValue().toString();
      }
    }

    /* make string buffer */

    buf.append( "<form>" );

    // Scenario
    /*
     * if( scenarioResult != null ) { buf.append( "<p>" ); buf.append( "<span color=\"header\" font=\"header\">" +
     * "Szenario: " + scenarioName + "</span>" ); buf.append( "</p>" );
     * 
     * buf.append( "<p>" ); buf.append( scenarioDescription ); buf.append( "</p>" ); }
     */
    // CalcUnit
    if( calcUnitResult != null )
    {
      buf.append( "<p>" );
      buf.append( "<b>Teilmodell " + calcUnitName + "</b>" );
      buf.append( "</p>" );
      //
      // buf.append( "<p>" );
      // buf.append( calcUnitDescription );
      // buf.append( "</p>" );

      buf.append( "<p>" );
      buf.append( "<b>Datum der Berechnung:</b>" );
      buf.append( "</p>" );

      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"Beginn:\">" + calcStart + "</li>" );
      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"Ende:\">" + calcEnd + "</li>" );
      buf.append( "<br/>\n\n" );
    }

    // Step
    if( stepResult != null )
    {
      buf.append( "<p>" );
      buf.append( "<b>Zeitschrittart:</b>" );
      buf.append( "</p>" );

      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"" + stepType + "\"></li>" );

      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"zum Zeitpunkt:\">" + stepTime + "</li>" );
      
      // Just to avoid to write -1 as steady "timestep" 
      if( !stepResult.getStepType().equals( IStepResultMeta.STEPTYPE.steady ) )
        buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"Schrittnummer:\">" + stepNumber + "</li>" );
      
      buf.append( "<br/>" );
    }

    // Document
    if( docResult != null )
    {
      buf.append( "<p>" );
      buf.append( "<b>Datentyp: </b>" );
      buf.append( "</p>" );

      buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"" + docType + "\"></li>" );

      if( docMax != null )
      {
        buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"maximaler Wert:\">" + docMax + "</li>" );
      }
      if( docMin != null )
      {
        buf.append( "<li style=\"text\" bindent=\"10\" indent=\"120\" value=\"minimaler Wert:\">" + docMin + "</li>" );
      }

    }

    buf.append( "</form>" );

    return buf.toString();
  }

  /**
   * gets the ScenarioResultMeta as the papa of all results
   */
  private IScenarioResultMeta getScenarioResultMeta( IResultMeta result )
  {
    if( result instanceof IScenarioResultMeta )
      return (IScenarioResultMeta) result;
    else
    {
      IResultMeta parent = result.getParent();
      if( parent != null )
      {
        return getScenarioResultMeta( parent );
      }
    }
    return null;
  }

  /**
   * gets the CalcUnitResultMeta as the papa of all steps
   */
  private ICalcUnitResultMeta getCalcUnitResultMeta( IResultMeta result )
  {
    if( result instanceof ICalcUnitResultMeta )
      return (ICalcUnitResultMeta) result;
    else
    {
      IResultMeta parent = result.getParent();
      if( parent != null )
      {
        return getCalcUnitResultMeta( parent );
      }
    }
    return null;
  }

  /**
   * gets the StepResultMeta as the papa of all documents (except tin_terrain)
   */
  private IStepResultMeta getStepResultMeta( IResultMeta result )
  {
    if( result instanceof IStepResultMeta )
      return (IStepResultMeta) result;
    else
    {
      IResultMeta parent = result.getParent();
      if( parent != null )
      {
        return getStepResultMeta( parent );
      }
    }
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#setInput(java.lang.Object)
   */
  @Override
  public void setInput( Object input )
  {
    m_input = input;

    refresh();
  }

  /**
   * @see org.eclipse.jface.viewers.Viewer#setSelection(org.eclipse.jface.viewers.ISelection, boolean)
   */
  @Override
  public void setSelection( ISelection selection, boolean reveal )
  {
    throw new UnsupportedOperationException();
  }

}
