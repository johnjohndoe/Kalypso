/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.ui.wizards.results;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.Date;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.project.Scenario1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IScenarioResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

import de.renew.workflow.connector.cases.IScenario;

/**
 * Creates the formatted (html) info text for a 1d2d result.
 * 
 * @author Gernot Belger
 */
public class ResultInfoBuilder
{
  private final DateFormat m_dateFormat = DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.LONG );

  public ResultInfoBuilder( )
  {
    m_dateFormat.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );
  }

  public String format( final Object element )
  {
    if( element == null )
      return "<form>" + Messages.getString( "org.kalypso.ui.wizards.results.ResultMetaInfoViewer.5" ) + "<form>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    final StringWriter buffer = new StringWriter();
    final PrintWriter printer = new PrintWriter( buffer );

    printInformation( element, printer );

    printer.flush();
    printer.close();

    return buffer.toString();
  }

  private void printInformation( final Object element, final PrintWriter printer )
  {
    printer.append( "<form>" ); //$NON-NLS-1$

    if( element instanceof IProject )
      printProject( (IProject)element, printer );
    else if( element instanceof IScenario )
    {
      final IFolder scenarioFolder = ((IScenario)element).getFolder();
      printScenario( scenarioFolder, printer );
    }
    else if( (element instanceof IResultMeta) )
      printChain( (IResultMeta)element, printer );

    printer.append( "</form>" ); //$NON-NLS-1$    
  }

  private void printChain( final IResultMeta result, final PrintWriter printer )
  {
    if( result == null )
      return;

    // Head recursion, to we start ultimatively with the root of the results and append the whole chain successively
    final IResultMeta owner = result.getOwner();
    printChain( owner, printer );

    printResult( result, printer );
  }

  private void printResult( final IResultMeta result, final PrintWriter printer )
  {
    if( result instanceof IScenarioResultMeta )
      printScenarioResult( (IScenarioResultMeta)result, printer );
    if( result instanceof ICalcUnitResultMeta )
      printCalcUnitResult( (ICalcUnitResultMeta)result, printer );
    else if( result instanceof IStepResultMeta )
      printStepResult( (IStepResultMeta)result, printer );
    else if( result instanceof IDocumentResultMeta )
      printDocumentResult( (IDocumentResultMeta)result, printer );
  }

  private void printScenarioResult( final IScenarioResultMeta result, final PrintWriter printer )
  {
    final Scenario1D2D scenario = ResultMeta1d2dHelper.findScenarioLocation( result );
    if( scenario != null )
    {
      final IFolder scenarioFolder = scenario.getScenarioFolder();
      printScenario( scenarioFolder, printer );
    }

    final String description = htmlString( result.getDescription() );
    if( !StringUtils.isBlank( description ) )
      printer.format( "<li style='text' bindent='10' indent='150' value='%s'></li>%n", description ); //$NON-NLS-1$ 
  }

  private void printScenario( final IFolder scenarioFolder, final PrintWriter printer )
  {
    printProject( scenarioFolder.getProject(), printer );

    final String name = scenarioFolder.getName();
    printer.format( "<p><b>%s:</b> %s</p>%n", "Szenario", name ); //$NON-NLS-1$
  }

  private void printProject( final IProject project, final PrintWriter printer )
  {
    final String projectName = project.getName();
    printer.format( "<p><b>%s:</b> %s</p>%n", "Projekt", projectName ); //$NON-NLS-1$
  }

  private void printCalcUnitResult( final ICalcUnitResultMeta result, final PrintWriter printer )
  {
    final String name = htmlString( result.getName() );
    printer.format( "<p><b>%s:</b> %s</p>%n", "Teilmodell", name ); //$NON-NLS-1$

    final String description = htmlString( result.getDescription() );
    if( !StringUtils.isBlank( description ) )
      printer.format( "<li style='text' bindent='10' indent='150' value='%s'></li>%n", description ); //$NON-NLS-1$ 

    final Date calcStartTime = result.getCalcStartTime();
    final Date calcEndTime = result.getCalcEndTime();

    final String calcStart = calcStartTime == null ? "-" : m_dateFormat.format( calcStartTime ); //$NON-NLS-1$
    final String calcEnd = calcEndTime == null ? "-" : m_dateFormat.format( calcEndTime ); //$NON-NLS-1$

    printer.format( "<li style='text' bindent='0' indent='10' value=''><b>%s</b></li>%n", "Datum der Berechnung", calcStart ); //$NON-NLS-1$ 

    printer.format( "<li style='text' bindent='10' indent='150' value='%s:'>%s</li>%n", "Beginn", calcStart ); //$NON-NLS-1$ 
    printer.format( "<li style='text' bindent='10' indent='150' value='%s:'>%s</li>%n", "Ende", calcEnd ); //$NON-NLS-1$ 

    printer.println( "<br/>" ); //$NON-NLS-1$
    printer.println();
  }

  private void printStepResult( final IStepResultMeta result, final PrintWriter printer )
  {
    String stepLabel;
    final Date stepResultTime = result.getStepTime();
    if( stepResultTime == null )
      stepLabel = result.getStepType().toString();
    else
      stepLabel = m_dateFormat.format( stepResultTime ); //$NON-NLS-1$

    printer.format( "<p><b>%s:</b></p>%n", "Zeitschritt" ); //$NON-NLS-1$
    printer.format( "<li style='text' bindent='10' indent='150' value='%s'></li>%n", stepLabel ); //$NON-NLS-1$ 

    printer.println( "<br/>" ); //$NON-NLS-1$
  }

  private void printDocumentResult( final IDocumentResultMeta result, final PrintWriter printer )
  {
    printer.format( "<p><b>%s:</b></p>%n", "Ergebnis" ); //$NON-NLS-1$

    final String description = htmlString( result.getDescription() );
    if( !StringUtils.isBlank( description ) )
      printer.format( "<li style='text' bindent='10' indent='150' value='%s'></li>%n", description ); //$NON-NLS-1$

    final String docType = result.getDocumentType().toString();
    printer.format( "<li style='text' bindent='10' indent='150' value='%s:'>%s</li>%n", "Datenart", docType ); //$NON-NLS-1$

    final String valueRangeHtml = formatValueRanges( result );
    if( !StringUtils.isBlank( valueRangeHtml ) )
    {
      printer.format( "<li style='text' bindent='10' indent='150' value='%s:'></li>%n", "Wertebereich", docType ); //$NON-NLS-1$
      printer.println( valueRangeHtml );
    }
  }

  private String formatValueRanges( final IDocumentResultMeta result )
  {
    final StringWriter buffer = new StringWriter();
    final PrintWriter printer = new PrintWriter( buffer );

    /* min - max (if set) */
    final BigDecimal docMin = result.getMinValue();
    final BigDecimal docMax = result.getMaxValue();
    // TODO: name is not ideal here, but works for most cases
    printMinMax( result.getName(), docMin, docMax, printer );

    /* min - max for each node type (if set) */
    for( final String resultType : NodeResultHelper.NodeStyleTypes )
    {
      if( resultType.equals( NodeResultHelper.WAVE_DIRECTION_TYPE ) )
        continue;

      final String parameterLabel = NodeResultHelper.translateNodeParameterType( resultType.toLowerCase() );

      final BigDecimal minValueForType = result.getMinValueForType( resultType );
      final BigDecimal maxValueForType = result.getMaxValueForType( resultType );
      printMinMax( parameterLabel, minValueForType, maxValueForType, printer );
    }

    printer.flush();
    printer.close();

    return buffer.toString();
  }

  private void printMinMax( final String label, final BigDecimal docMin, final BigDecimal docMax, final PrintWriter printer )
  {
    if( docMin == null && docMax == null )
      return;

    final String minText = docMin == null ? "?" : docMin.toString();
    final String maxText = docMax == null ? "?" : docMax.toString();
    printer.format( "<li style='text' bindent='20' indent='150' value='%s'>%s - %s</li>%n", label, minText, maxText ); //$NON-NLS-1$
  }

  // FIXME: move to helper
  private static String htmlString( final String text )
  {
    final String result = StringUtils.replace( text, "\"", "&quot;" );

    return StringUtils.replace( result, "'", "&apos;" );
  }
}