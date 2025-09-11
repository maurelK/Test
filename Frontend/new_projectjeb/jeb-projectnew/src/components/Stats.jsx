import './Stats.css'
import { useEffect, useState, useRef } from 'react'

function Stat({ target, label, trigger }) {
  const [count, setCount] = useState(0)

  useEffect(() => {
    if (!trigger) return

    let current = 0
    const end = typeof target === 'number' ? target : parseInt(target)
    const duration = 1000
    const step = Math.max(Math.floor(duration / end), 10)

    const timer = setInterval(() => {
      current += 1
      setCount(current)
      if (current >= end) clearInterval(timer)
    }, step)

    return () => clearInterval(timer)
  }, [trigger, target])

  return (
    <div className="stat-item">
      <span className="stat-number">{count}+</span>
      <span className="stat-label">{label}</span>
    </div>
  )
}

function Stats() {
  const [visible, setVisible] = useState(false)
  const statsRef = useRef(null)

  useEffect(() => {
    const observer = new IntersectionObserver(
      ([entry]) => {
        if (entry.isIntersecting) setVisible(true)
      },
      { threshold: 0.5 }
    )

    if (statsRef.current) observer.observe(statsRef.current)

    return () => {
      if (statsRef.current) observer.unobserve(statsRef.current)
    }
  }, [])

  return (
    <div className="stats-bar" ref={statsRef}>
      <div className="stats-container">
        <Stat target={120} label="Projets incubés" trigger={visible} />
        <Stat target={35} label="Partenaires" trigger={visible} />
        <Stat target={10} label="Ans d'expérience" trigger={visible} />
      </div>
    </div>
  )
}

export default Stats
