package org.kalypso.ogc.sensor.filter;

import org.kalypso.ogc.sensor.IObservation;

/**
 * IObservationFilter
 * <p>
 * The general syntax for a filter is
 * 
 * <pre>
 * filter(&lt;filter_string&gt;)
 * </pre>.
 * <p>
 * syntax of the filter specification in the fragment part of the URL. Here is
 * the syntax:
 * 
 * <pre>
 * file://path/resource.ext#filter(ID*CONF)
 *            
 * ID: Alphanum
 *     id of the filter to use
 *            
 * CONF: Alphanum and following chars: - _ . ! &tilde; '
 *       configuration string for the filter
 * </pre>
 *
 * TODO: IMPORTANT NOTE: currently the 'fragment' separator is not '#' but '?'. 
 * 
 * @author schlienger
 */
public interface IObservationFilter extends IObservation
{
  public void initFilter( final String conf, final IObservation obs );
}