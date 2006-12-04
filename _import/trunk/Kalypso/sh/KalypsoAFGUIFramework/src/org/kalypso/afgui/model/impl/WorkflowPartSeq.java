package org.kalypso.afgui.model.impl;

import java.lang.reflect.Array;
import java.lang.reflect.TypeVariable;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.kalypso.afgui.model.ITaskGroup;
import org.kalypso.afgui.model.IWorkPartSeq;
import org.kalypso.afgui.model.IWorkflowPart;
import org.kalypso.afgui.schema.Schema;

import com.hp.hpl.jena.rdf.model.NodeIterator;
import com.hp.hpl.jena.rdf.model.Resource;
import com.hp.hpl.jena.rdf.model.Seq;

/**
 * @author Patrice Congo
 *
 */
public class WorkflowPartSeq<E extends IWorkflowPart> implements IWorkPartSeq<E>
{
	protected Seq seq;
	protected Class<E> c;
	
	public WorkflowPartSeq(Seq seq, Class<E> c)
	{
		//assertNotNull(seq);
		this.seq=seq;
		this.c=c;
	}
	
	final static private int toSeqIndex(int index)
	{
		return index+1;
	}
	final static private void assertIndexInBounds(int index, int size)
	{
		if(index>=size || index<0)
		{
			throw new ArrayIndexOutOfBoundsException();
		}
		
	}

	final static private void assertNotNull(Object o)
	{
		if(o==null)
		{
			throw new NullPointerException();
		}
	}
	
	public boolean add(E o)
	{
		
		Resource res=(Resource)o.getModelObject();
		if(res==null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			seq.add(res);
			return false;
		}
	}

	public void add(int index, E element)
	{
		assertIndexInBounds(index, size());
		assertNotNull(element);
		Resource res=(Resource)element.getModelObject();
		if(res==null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			seq.add(toSeqIndex(index),res);
		}
	}

	public boolean addAll(Collection<? extends E> c)
	{
		throw new UnsupportedOperationException();
	}

	public boolean addAll(int index, Collection<? extends E> c)
	{
		throw new UnsupportedOperationException();
	}

	public void clear()
	{
		for(int i=size();i>0;i--)
		{
			seq.remove(i);
		}
	}

	public boolean contains(Object o)
	{
		if(o==null)
		{
			throw new NullPointerException();
		}
		else
		{
			if(c.isInstance(o))
			{
				Resource res=(Resource)((E)o).getModelObject();
				if(res==null)
				{
					throw new IllegalArgumentException();
				}
				else
				{
					return seq.contains(res);
				}
			}
			else
			{
				return false;
			}
		}
		
	}

	public boolean containsAll(Collection<?> c)
	{
		if(c==null)
		{
			throw new NullPointerException();
		}
		else
		{
			for(Object cEl:c)
			{
				if(!contains(cEl))
				{
					return false;
				}
			}
			return true;
		}
		
	}

	public E get(int index)
	{
		assertIndexInBounds(index, size());
		Resource res=seq.getResource(toSeqIndex(index));
		return Schema.asWorkflowPart(res, c);
	}

	public int indexOf(Object o)
	{
		if(o==null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			if(c.isInstance(o))
			{
				Resource res=(Resource)((E)o).getModelObject();
				if(res==null)
				{
					return seq.indexOf(res)-1;
				}
				else
				{
					return -1;
				}
			}
			else
			{
				return -1;
			}
		}
	}

	public boolean isEmpty()
	{
		return size()==0;
	}

	public Iterator<E> iterator()
	{
		return new Iterator<E>()
		{
			final private NodeIterator nit= seq.iterator();
			
			public boolean hasNext()
			{
				return nit.hasNext();
			}

			public E next()
			{
				Resource res= (Resource)nit.next();
				return Schema.asWorkflowPart(res, c);
			}

			public void remove()
			{
				nit.remove();				
			}
			
		};
	}

	public int lastIndexOf(Object o)
	{
		//return 0;
		throw new UnsupportedOperationException();
	}

	public ListIterator<E> listIterator()
	{
		throw new UnsupportedOperationException();
	}

	public ListIterator<E> listIterator(int index)
	{
		throw new UnsupportedOperationException();
	}

	public boolean remove(Object o)
	{
		assertNotNull(o);
		if(c.isInstance(o))
		{
			Resource res= (Resource)((E)o).getModelObject();
			if(res==null)
			{
				throw new IllegalArgumentException();
			}
			else
			{
				int seqIndex=seq.indexOf(res);
				if(seqIndex>0)
				{
					seq.remove(seqIndex);
					return true;
				}
				else
				{
					return false;
				}
			}
		}
		else
		{
			return false;
		}
	}

	public E remove(int index)
	{
		assertIndexInBounds(index, size());
		Resource res=seq.getResource(toSeqIndex(index));
		seq.remove(toSeqIndex(index));
		if(res==null)
		{
			return null;
		}
		else
		{
			return (E) Schema.asWorkflowPart(res, c);
		}
	
	}

	public boolean removeAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	public boolean retainAll(Collection<?> c)
	{
		throw new UnsupportedOperationException();
	}

	public E set(int index, E element)
	{
		assertIndexInBounds(index, size());
		int seqIndex=toSeqIndex(index);
		Resource oldRes=seq.getResource(seqIndex);
		Resource newRes=(Resource)element.getModelObject();
		if(newRes==null)
		{
			throw new IllegalArgumentException();
		}
		else
		{
			seq.set(seqIndex, newRes);
			return Schema.asWorkflowPart(oldRes, c);
		}
		
	}

	public int size()
	{
		return seq.size();
	}

	public List<E> subList(int fromIndex, int toIndex)
	{
		throw new UnsupportedOperationException();
	}

	public Object[] toArray()
	{
		Object[] objs= new Object[size()];
		for(int i=0;i<objs.length;i++)
		{
			objs[i]=get(i);
		}
		return objs;
	}

	public <T> T[] toArray(T[] a)
	{
		final int SIZE=size();
		if(a.length >SIZE)
		{
			Class cType=a.getClass().getComponentType();
			a= (T[])Array.newInstance(cType, SIZE);
		}
		
		try
		{
			for(int i=0;i<SIZE;i++)
			{
				a[i]=(T)get(i);
			}
			return a;
		}
		catch(ClassCastException e)
		{
			throw new ArrayStoreException();
		}
	
	}
	
	static public void main(String[] args)
	{
		WorkflowPartSeq<ITaskGroup> seq = new WorkflowPartSeq<ITaskGroup>(null,ITaskGroup.class);
		
		TypeVariable tv[]=	seq.getClass().getTypeParameters();
		System.out.println(seq.c);
	}
}
