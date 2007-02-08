/**
 * 
 */
package org.kalypso.afgui.model.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.core.runtime.Platform;
import org.kalypso.afgui.model.EActivityRelationship;
import org.kalypso.afgui.model.IActivitySpecification;
import org.kalypso.afgui.model.IRelationshipStatement;
import org.kalypso.afgui.model.IWorkflowSpecification;

/**
 * The default implementation of {@link IWorkflowSpecification}.
 * 
 * @author Patrice Congo
 */
public class WorkflowSpecification implements IWorkflowSpecification
{
	final static private Logger logger= 
			Logger.getLogger(WorkflowSpecification.class.getName());
     private static final boolean log = Boolean.parseBoolean( Platform.getDebugOption( "org.kalypso.afgui/debug" ) );

        static
        {
          if( !log )
            logger.setUseParentHandlers( false );
        }
        
	final private Map<String, IActivitySpecification> specs;
	
	final private Map<	EActivityRelationship, 
						List<IRelationshipStatement>> statements;
	
	final private List<IActivitySpecification> rootSpecs;
	
	public WorkflowSpecification(
			Map<String, IActivitySpecification> specs,
			List<IRelationshipStatement> stms)
	{
		logger.info("\n=========new WorkflowSpecification\n"+stms+"\n--\n"+specs);
		this.specs= Collections.unmodifiableMap(specs);
		this.statements= toStatementMap(stms);
		
		List<IActivitySpecification> rsCache=new ArrayList<IActivitySpecification>();
		for(IActivitySpecification spec:specs.values())
		{
			logger.info("\nScpecName="+spec.getName()+" isRoot:"+spec.isRoot());
			if(spec.isRoot())
			{
				rsCache.add(spec);
			}
		}
		this.rootSpecs=Collections.unmodifiableList(rsCache);
		
		
	}
	
	
	private static final 
		Map<EActivityRelationship, List<IRelationshipStatement>> 
								toStatementMap(List<IRelationshipStatement> stmList)
	{
		
		Map<EActivityRelationship,List<IRelationshipStatement>> stmsMap= 
			new HashMap<EActivityRelationship, List<IRelationshipStatement>>();
		//rel lists for actions
		for(EActivityRelationship relKey: EActivityRelationship.values())
		{
			stmsMap.put(relKey, new ArrayList<IRelationshipStatement>());
		}
		
		for(IRelationshipStatement stm: stmList)
		{
			stmsMap.get(stm.getPredicate()).add(stm);
		}
		
		stmsMap.get(EActivityRelationship.ANY).addAll(stmList);
		return stmsMap;
	}
	
	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowSpecification#getActivitySpecificationByName(java.lang.String)
	 */
	public IActivitySpecification getActivitySpecificationByName(String name)
	{
		if(name==null)
		{
			throw new IllegalArgumentException(
					"parameter name must not be null");
		}
		
		return specs.get(name);
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowSpecification#getLinkedActivitySpecs(org.kalypso.afgui.model.IActivitySpecification, org.kalypso.afgui.model.EActivityRelationship)
	 */
	public List<IActivitySpecification> getLinkedActivitySpecs(
												IActivitySpecification spec, 
												EActivityRelationship aRelationship)
	{
		logger.info(spec.toString());
		logger.info(aRelationship.toString());
		logger.info(statements.toString());
		
		List<IRelationshipStatement> rels= statements.get(aRelationship);
		if(spec==null)
		{
			List<IActivitySpecification> actSpecs=
					new ArrayList<IActivitySpecification>(rels.size());
			for(IRelationshipStatement rel: rels)
			{
				actSpecs.add(rel.getObject());
			}
			return actSpecs;
		}
		else
		{
			List<IActivitySpecification> actSpecs=
				new ArrayList<IActivitySpecification>(rels.size());
//			IActivitySpecification subject;
			for(IRelationshipStatement rel: rels)
			{
				
				if( (spec==rel.getSubject()))
				{
					actSpecs.add(rel.getObject());
				}
			}
		
			return actSpecs;
		}
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowSpecification#getRootActivitySpecs()
	 */
	public List<IActivitySpecification> getRootActivitySpecs()
	{
		return rootSpecs;
	}

	/* (non-Javadoc)
	 * @see org.kalypso.afgui.model.IWorkflowSpecification#selectRelationships(org.kalypso.afgui.model.IActivitySpecification, org.kalypso.afgui.model.EActivityRelationship, org.kalypso.afgui.model.IActivitySpecification)
	 */
	public List<IRelationshipStatement> selectRelationships(
												IActivitySpecification subjectSpec,
												EActivityRelationship predicate, 
												IActivitySpecification objectSpec)
	{
		List<IRelationshipStatement> stms=statements.get(predicate);
		IRelationshipStatement stm;
		for(Iterator<IRelationshipStatement> it=stms.iterator();it.hasNext();)
		{
			stm=it.next();
			if(subjectSpec!=null)
			{
				if(subjectSpec.equals(stm.getSubject()))
				{
					it.remove();
					continue;
				}
			}
			else if(objectSpec!=null)
			{
				if(objectSpec.equals(stm.getObject()))
				{
					it.remove();
					continue;
				}
			}
			else
			{
				//empty
			}
		}
		return stms;
	}
	@Override
	public String toString()
	{ 
		StringBuffer buffer= new StringBuffer(256);
		buffer.append("\n**********");
		buffer.append(specs.values());
		
		return buffer.toString();
	}

}
