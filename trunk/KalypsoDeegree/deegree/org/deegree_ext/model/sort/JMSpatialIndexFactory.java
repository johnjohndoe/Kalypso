package org.deegree_ext.model.sort;
import org.deegree.model.geometry.GM_Envelope;

public class JMSpatialIndexFactory
{
 
    public static JMSpatialIndex createSpatialIndex(GM_Envelope env)
    {
	return (JMSpatialIndex)new SplitSort(env);
    }

}
