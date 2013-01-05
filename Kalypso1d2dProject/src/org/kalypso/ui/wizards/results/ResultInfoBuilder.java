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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
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
      return "<form>" + Messages.getString( "org.kalypso.ui.wizards.results.ResultMetaInfoViewer.5" ) + "</form>"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

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
      final IScenario scenario = (IScenario)element;
      final IFolder scenarioFolder = scenario.getFolder();
      printScenario( scenarioFolder, printer );

      printDescription( scenario.getDescription(), printer );
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

    printDescription( result.getDescription(), printer );
  }

  private void printScenario( final IFolder scenarioFolder, final PrintWriter printer )
  {
    printProject( scenarioFolder.getProject(), printer );

    final String name = scenarioFolder.getName();
    printer.format( "<p><b>%s:</b> %s</p>%n", Messages.getString( "ResultInfoBuilder.0" ), name ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  private void printProject( final IProject project, final PrintWriter printer )
  {
    final String projectName = project.getName();
    printer.format( "<p><b>%s:</b> %s</p>%n", Messages.getString( "ResultInfoBuilder.1" ), projectName ); //$NON-NLS-1$ //$NON-NLS-2$

    try
    {
      printDescription( project.getDescription().getComment(), printer );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
  }

  private void printCalcUnitResult( final ICalcUnitResultMeta result, final PrintWriter printer )
  {
    final String name = htmlString( result.getName() );
    printer.format( "<p><b>%s:</b> %s</p>%n", Messages.getString( "ResultInfoBuilder.2" ), name ); //$NON-NLS-1$ //$NON-NLS-2$

    printDescription( result.getDescription(), printer );

    final Date calcStartTime = result.getCalcStartTime();
    final Date calcEndTime = result.getCalcEndTime();

    final String calcStart = calcStartTime == null ? "-" : m_dateFormat.format( calcStartTime ); //$NON-NLS-1$
    final String calcEnd = calcEndTime == null ? "-" : m_dateFormat.format( calcEndTime ); //$NON-NLS-1$

    printer.format( "<li style='text' bindent='0' indent='10' value=''><b>%s</b></li>%n", Messages.getString( "ResultInfoBuilder.3" ), calcStart ); //$NON-NLS-1$ //$NON-NLS-2$ 

    printer.format( "<li style='text' bindent='10' indent='150' value='%s:'>%s</li>%n", Messages.getString( "ResultInfoBuilder.4" ), calcStart ); //$NON-NLS-1$ //$NON-NLS-2$ 
    printer.format( "<li style='text' bindent='10' indent='150' value='%s:'>%s</li>%n", Messages.getString( "ResultInfoBuilder.5" ), calcEnd ); //$NON-NLS-1$ //$NON-NLS-2$ 

    printer.println( "<br/>" ); //$NON-NLS-1$
    printer.println();
  }

  private void printStepResult( final IStepResultMeta result, final PrintWriter printer )
  {
    final String stepLabel = formatStepLabel( result );

    printer.format( "<p><b>%s:</b></p>%n", Messages.getString( "ResultInfoBuilder.6" ) ); //$NON-NLS-1$ //$NON-NLS-2$
    printer.format( "<li style='text' bindent='10' indent='150' value='%s'></li>%n", stepLabel ); //$NON-NLS-1$ 

    printer.println( "<br/>" ); //$NON-NLS-1$
  }

  public String formatStepLabel( final IStepResultMeta result )
  {
    final Date stepResultTime = result.getStepTime();
    if( stepResultTime == null )
      return result.getStepType().toString();
    else
      return m_dateFormat.format( stepResultTime ); //$NON-NLS-1$
  }

  private void printDocumentResult( final IDocumentResultMeta result, final PrintWriter printer )
  {
    printer.format( "<p><b>%s:</b></p>%n", Messages.getString( "ResultInfoBuilder.7" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    printDescription( result.getDescription(), printer );

    final String docType = result.getDocumentType().toString();
    printer.format( "<li style='text' bindent='10' indent='150' value='%s:'>%s</li>%n", Messages.getString( "ResultInfoBuilder.8" ), docType ); //$NON-NLS-1$ //$NON-NLS-2$

    final String valueRangeHtml = formatValueRanges( result );
    if( !StringUtils.isBlank( valueRangeHtml ) )
    {
      printer.format( "<li style='text' bindent='10' indent='150' value='%s:'></li>%n", Messages.getString( "ResultInfoBuilder.9" ), docType ); //$NON-NLS-1$ //$NON-NLS-2$
      printer.println( valueRangeHtml );
    }
  }

  private void printDescription( final String description, final PrintWriter printer )
  {
    if( StringUtils.isBlank( description ) )
      return;

    final String htmlText = htmlStringBreaklines( description );
    printer.format( "<li style='text' bindent='0' indent='10' value=''>%s</li>%n", htmlText ); //$NON-NLS-1$
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

    final String minText = docMin == null ? "?" : docMin.toString(); //$NON-NLS-1$
    final String maxText = docMax == null ? "?" : docMax.toString(); //$NON-NLS-1$
    printer.format( "<li style='text' bindent='20' indent='150' value='%s'>%s - %s</li>%n", label, minText, maxText ); //$NON-NLS-1$
  }

  // FIXME: move to helper
  private static String htmlString( final String text )
  {
    final String result = StringUtils.replace( text, "\"", "&quot;" ); //$NON-NLS-1$ //$NON-NLS-2$

    return StringUtils.replace( result, "'", "&apos;" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * Additionally replaces breaklines with '<br/>
   * , can only be used for text nodes.
   */
  private static String htmlStringBreaklines( final String text )
  {
    final String htmlText = htmlString( text );
    return StringUtils.replace( htmlText, "\n", "<br/>" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public String formatResultLabel( final IResultMeta result, final IFolder currentScenario )
  {
    final Collection<String> buffer = new ArrayList<>( 5 );

    final Pair<IProject, IFolder> externalLocation = ResultMeta1d2dHelper.determineExternalLocation( result, currentScenario );

    final IProject externalProject = externalLocation.getLeft();
    if( externalProject != null )
      buffer.add( externalProject.getName() );

    final IFolder externalScenario = externalLocation.getRight();
    if( externalScenario != null )
      buffer.add( externalScenario.getName() );

    /* recurively add his element and all owners */
    addNameChain( buffer, result );

    return StringUtils.join( buffer, " - " ); //$NON-NLS-1$
  }

  private void addNameChain( final Collection<String> buffer, final IResultMeta result )
  {
    final IResultMeta owner = result.getOwner();
    if( owner != null && !(owner instanceof IScenarioResultMeta) )
      addNameChain( buffer, owner );

    buffer.add( result.getName() );
  }
}