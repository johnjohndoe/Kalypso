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

import java.math.BigDecimal;
import java.text.DateFormat;
import java.util.Date;
import java.util.Map;

import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.NodeResultHelper;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultMeta1d2dHelper;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.ICalcUnitResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IStepResultMeta;
import org.kalypso.kalypsosimulationmodel.core.resultmeta.IResultMeta;

/**
 * Creates the formatted (html) info text for a 1d2d result.
 * 
 * @author Gernot Belger
 */
public class ResultInfoBuilder
{
  public String format( final Object element )
  {
    if( element == null )
      return Messages.getString( "org.kalypso.ui.wizards.results.ResultMetaInfoViewer.5" ); //$NON-NLS-1$

    if( !(element instanceof IResultMeta) )
      return null;

    final IResultMeta result = (IResultMeta)element;

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

    // TODO: uäargh! Lots of copy/paste code....!
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
  // FIXME: move to helper
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
}