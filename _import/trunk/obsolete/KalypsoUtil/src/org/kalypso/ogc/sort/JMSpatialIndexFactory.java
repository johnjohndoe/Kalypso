package org.kalypso.ogc.sort;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.sort.JMSpatialIndex;

public class JMSpatialIndexFactory
{
 
    public static JMSpatialIndex createSpatialIndex(GM_Envelope env)
    {
	return (JMSpatialIndex)new SplitSort(env);
    }

}
