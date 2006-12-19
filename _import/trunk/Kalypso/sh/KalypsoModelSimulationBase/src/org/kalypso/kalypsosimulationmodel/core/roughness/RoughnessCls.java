package org.kalypso.kalypsosimulationmodel.core.roughness;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * Default implementation for {@link IRoughnessCls} which 
 * serves as a wrapper arround a wbd:Roughness feature
 * 
 * @author Patrice Congo
 *
 */
public class RoughnessCls implements IRoughnessCls
{
	private final Feature feature;
	
	/**
	 * Create a Roughness object wrapping the given feature.
	 * 
	 * @param feature - the wbr:Roughness feature to wrapped
	 * @throw IllegalArgumentException if feature is null or
	 * 	is note a wbr:Roughness feature 
	 * 		
	 * 
	 */
	public RoughnessCls(
				Feature feature) 
				throws IllegalArgumentException
	{
		this.feature=feature;
	}
	
	/**
	 * Creates a feature into the given workspace.
	 * The feature is added as roughnessMember into
	 * the roughness collection which needs to be the
	 * root of the workspace
	 * 
	 * @param workspace -- the gml  workspace to addd the
	 *  feature into
	 * @throws IllegalArgumentException if workspace is null
	 *  or workspace root elemment is no a RoughnessCollection
	 */
	public RoughnessCls(
				GMLWorkspace workspace)
				throws IllegalArgumentException
	{
		this.feature=null;
	}
	
	/**
	 * Creates a feature into the given workspace as roughnessMember into
	 * the specified roughness collection. The roughness collection here 
	 * must not be the  root of the workspace but is only required to  be 
	 * part of the workspace.
	 * 
	 * @param workspace -- the gml  workspace to addd the
	 *  feature into
	 * @throws IllegalArgumentException if workspace is null
	 *  or the roughness collection is not part of the workspace
	 */
	public RoughnessCls(
				GMLWorkspace workspace,
				Feature roughnessCollection)
				throws IllegalArgumentException
	{
		this.feature=null;
	}
	
	
	
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getAxAy()
	 */
	public double getAxAy()
	{
		
		return 0;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getDp()
	 */
	public double getDp()
	{
		return 0;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getEddy()
	 */
	public double getEddy()
	{
		return 0;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getKs()
	 */
	public double getKs()
	{
		return 0;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#set(double, double, double, double)
	 */
	public RoughnessConfigConsistency set(double ks, double axay, double dp,
			double eddy)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setAxAy(double)
	 */
	public void setAxAy(double axay) throws IllegalArgumentException
	{
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setDp(double)
	 */
	public void setDp(double dp) throws IllegalArgumentException
	{
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setEddy(double)
	 */
	public void setEddy(double eddy) throws IllegalArgumentException
	{
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setKs(double)
	 */
	public void setKs(double ks) throws IllegalArgumentException
	{
		
	}

	///Ask nico for details
	//one is sure negatives are not allowed 
	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#validate()
	 */
	public RoughnessConfigConsistency validate()
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#configure(java.lang.String, java.lang.String, double, double, double, double)
	 */
	public RoughnessConfigConsistency configure(
											String URI, 
											String name, 
											double ks, 
											double axay, 
											double dp, 
											double eddy)
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getName()
	 */
	public String getName()
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getURI()
	 */
	public String getURI()
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setName(java.lang.String)
	 */
	public void setName(
					String name) 
					throws IllegalArgumentException
	{
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setURI(java.lang.String)
	 */
	public void setURI(
				String uri)
				throws IllegalArgumentException
	{
		
		
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#getDescription()
	 */
	public String getDescription()
	{
		return null;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.kalypsosimulationmodel.core.roughness.IRoughness#setDescription(java.lang.String)
	 */
	public void setDescription(String descriptionText)
	{
		
	}

	
	
	

}
